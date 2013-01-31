{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , MultiWayIf
           , TupleSections
  #-}

module Jimple.Maps where

import Debug.Trace

import qualified Parser as CF

import Control.Arrow (first, second)
import Control.Applicative ((<$>))

import Control.Monad
import qualified Control.Monad.State  as ST
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Error  as E

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe
import Data.Either

import Jimple.Types
import Jimple.Rewrite

import Text.Parsec     (getPosition, optionMaybe, try, many1)
import Text.Parsec.Pos (sourceLine)


-- Map through statements recursively
mapS f = map go
  where
    go (mlbl, stmt) = f (mlbl, stmt')
      where
        stmt' = case stmt of
          SIfElse  cnd  l r  -> SIfElse  cnd    (map go l) (map go r)
          SDoWhile name b c  -> SDoWhile name   (map go b) c
          SSwitch  name v cs -> SSwitch  name v [(c, map go s) | (c, s) <- cs]
          _                   -> stmt

-- Like mapS but returns values of f isntead of replacing inside the structure
mapA f ls = map f $ foldS (flip (:)) [] ls

-- Fold through statements recursively
foldS f = F.foldl' go
  where
    go acc0 (mlbl, stmt) = acc2
      where
        acc1 = f acc0 (mlbl, stmt)
        acc2 = case stmt of
          SIfElse  _ l r  -> F.foldl' go (F.foldl' go acc1 l) r
          SDoWhile _ b _  -> F.foldl' go acc1 b
          SSwitch  _ _ cs -> F.foldl' go acc1 $ concatMap snd cs
          _                -> acc1


-- Apply map until a fix-point is reached
mapFix f v = snd $ head $ dropWhile (uncurry (/=)) $ zip l $ tail l
  where
    l = iterate f v


-- StringBuilder.append("") does nothing
mapAppendEmpty m = m { methodStmts = map go $ methodStmts m }
  where
    go (l, SAssign r (VExpr (EInvoke (IVirtual v) sig [VConst (CString "")])))
      | methodClass  sig == CF.Class "java/lang/StringBuilder" &&
        methodName   sig == "append" &&
        methodParams sig == [TObject "java/lang/String"] =
          (l, SAssign r v)

    go s = s

-- Identify pure values (no side-effects, used for inlining and cleaning)
pureValue (VExpr EInvoke{}) = False
pureValue (VExpr (ENew r _args)) = case r of
  RInstanceField _ _ -> True
  RStaticField   _ _ -> True
  _ -> False
pureValue _ = True


-- Clean up dead code by removing pure values that aren't used
mapCleanup m = m { methodStmts = go $ methodStmts m }
  where
    go s = reverse $ catMaybes $ ST.evalState (mapM go' $ reverse s) S.empty

    -- Remove lines that do nothing (empty assign and nop)
    go' (mlbl, SAssign (VarLocal (Local "_")) (VLocal _)) =
      return $ Just (mlbl, SNop)
    go' (Nothing, SNop) = return Nothing

    -- Remove lines that aren't used elsewhere
    go' (l@(label, s@(SAssign (VarLocal v@(Local ('s':_))) e))) = do
      alive <- ST.gets $ S.member v
      when alive $ addAlive s
      let canRemove = not alive && pureValue e
      case label of
        Nothing | canRemove -> return Nothing
        Just _  | canRemove -> return $ Just (label, SNop)
        _ -> return $ Just l

    -- Keep the rest
    go' (l@(_, st)) = addAlive st >> return (Just l)

    addAlive = ST.modify . S.union . F.foldl f S.empty
      where
        f s (VLocal (VarLocal v)) = S.insert v s
        f s (VLocal (VarRef   r)) = F.foldl f s r
        f s (VExpr  e) = F.foldl f s e
        f s (VConst _) = s


-- Perform value-inlining of pure values
mapInline m = m { methodStmts = go $ methodStmts m }
  where
    go s = ST.evalState (mapM go' s) M.empty

    go' (l, s) = do
      m <- ST.get
      update s
      return (l, inline m `fmap` s)

    inline m (val@(VLocal (var@(VarLocal (Local ('s':_)))))) =
      fromMaybe val $ M.lookup var m
    inline m (VExpr  e) = VExpr $ inline m `fmap` e
    inline m (VLocal v) = VLocal $ inline m `fmap` v
    inline m e = e

    update (SAssign v e) | pureValue e = ST.modify $ M.insert v e
    update _                            = return ()


-- Rewrite labels from relative to absolute, while removing unused ones.
mapLabels m = m { methodStmts = go $ methodStmts m }
  where
    go s = map f s'
      where
       (s', labels) = W.runWriter $ mapM go' s

       f (Just l, s) | l `elem` labels = (Just l,  s)
       f (_     , s)                   = (Nothing, s)

    go' (Just pos, SIf c next) = tellLabel (SIf c) pos next
    go' (Just pos, SGoto next) = tellLabel SGoto   pos next
    go' (Just pos, SLookupSwitch v lbl cs) = do
      let cs'  = map (second (+pos)) cs
      let lbl' = pos + lbl
      W.tell $ lbl' : map snd cs'
      return (Just pos, SLookupSwitch v lbl' cs')

    -- TODO: STableSwitch
    go' s = return s


    tellLabel f (Label pos) (Label next) = do
      W.tell [Label $! pos + next]
      return (Just $ Label pos, f $ Label $ pos + next)


-- Move argument to init-calls into their respective new call (constructor)
mapInit m = m { methodStmts = go $ methodStmts m }
  where
    go s = catMaybes $ ST.evalState (mapM goM s) M.empty

    goM stmt@(lbl, s) = case s of
      SAssign v (VExpr (ENew (RObject cl) [])) -> do
        ST.modify $ M.insert cl v
        return Nothing

      SAssign _ (VExpr
                  (EInvoke
                   (ISpecial (VLocal v))
                   (MethodSig cl "<init>" _fs _ts _rt) pars)) -> do
        realv <- fromMaybe v <$> ST.gets (M.lookup cl)
        return $ Just (lbl, SAssign realv (VExpr (ENew (RObject cl) pars)))

      _ -> return $ Just stmt

-- Fix use of this reference
mapThis = mapRewritePrefix $ do
  first@(mlblS, SAssign v (VLocal (VarRef RThis))) <- anyStmt
  rest <- many anyStmt
  return $ first : (ST.evalState (mapM track rest) $ S.singleton v)
  where
    track stmt = do
      s <- ST.get
      case stmt of
        (mlbl, SAssign v' (VLocal v)) | S.member v s -> do
          ST.modify $ S.insert v
          return $ (mlbl, SAssign v' (VLocal $ VarRef RThis))

        _ -> return stmt


mapSuper = mapRewrite $ do
  (mlbl, SAssign (VarLocal (Local "_"))
         (VExpr (EInvoke
                 (ISpecial (VLocal _))
                 m@(MethodSig _cl "<init>" _fs _ts _rt) pars)
         )) <- anyStmt
  return []

-- Eliminate cross goto:
-- > goto 2
-- > 1: ...
-- > goto 3:
-- > 2: ...
-- > goto 1
-- > 3:
-- ==>
-- > 1: ...
-- > 2: ...
-- > 3:
mapElimGoto = mapRewrite $ do
  SGoto lbl2 <- gotoP
  body1Top@(Just lbl1, _) <- label

  body1 <- many jumpless

  SGoto lbl3 <- gotoP
  body2Top@(Just lbl2', _) <- label
  guard $ lbl2 == lbl2'

  body2 <- many jumpless

  SGoto lbl1' <- gotoP
  guard $ lbl1 == lbl1'

  -- check ending (not counted)
  body3Top@(Just lbl3', _) <- label
  guard $ lbl3 == lbl3'

  -- rearrange blocks without gotos
  let body = (body2Top:body2) ++ (body1Top:body1)

  return body


-- Try to clean up if-statements:
-- > ifLbl: if cond lbl1
-- > ... body1
-- { > lbl1: } ==> if cond [] body1
-- { > goto lbl2
--   > lbl1:
--   > ... body2
--   > lbl2: } ==> if cond body2 body1
mapGotoIf = mapRewrite $ do
  (ifLbl, SIf cond lbl1) <- ifP
  body1 <- bodyM lbl1
  next  <- anyStmt
  case next of
    -- if with else part: if cond 1 ... goto 2, 1: ... 2:
    (_, SGoto lbl2) -> do
      body2 <- bodyM lbl2
      return [(ifLbl, SIfElse cond body2 body1)]

    -- if with no else
    (Just lbl', _) | lbl' == lbl1 ->
      return [(ifLbl, SIfElse cond [] body1), next]

    _ -> E.throwError "mapGotoIf: mismatch"

  where
    bodyS lbl (_, SGoto _) = False
    bodyS lbl (mlbl,     _) = mlbl /= Just lbl

    bodyM lbl = many $ satisfy $ bodyS lbl


-- All labels with backwards jumps are loops
-- while:
-- > whileLbl: if cond lblOut
-- > ... body1
-- > goto whileLbl
-- > lblOut
--
-- do-while:
-- > whileLbl:
-- > ... body1
-- > if cond whileLbl
--
-- body1 may contain other loops/ifs, break and continue
mapWhile  m = m { methodStmts = go $ methodStmts m }
  where
    go ops = fromMaybe ops $ rewrite rule ops

    rule = do
      (Just lblStart, stmtStart) <- anyStmt
      let name = "while_" ++ show lblStart

      case M.lookup lblStart backrefs of
        Just (i, (j, stmt'):rs) | i < j -> do
          -- Extract loop body and final loop-jump
          body <- replicateM (j - i - 1) anyStmt
          (lblEnd, stmtEnd)  <- jumpP
          -- Sanity check
          E.guard (jumpLabel stmtEnd == Just lblStart)

          next@(lblNext, stmtNext) <- anyStmt
          -- Setup break/continue statements
          let labels = maybe id (`M.insert` SBreak name) lblNext $
                       M.fromList [(lblStart, SContinue name :: Stmt Value)]
          -- Rewrite labels inside body
          let body' = (Nothing, stmtStart) : replaceLabels labels body
          let cnd = case stmtEnd of
                SGoto _    -> VConst $ CBoolean True
                SIf cnd' _ -> VExpr cnd'
          -- Construct doWhile
          return [(Just lblStart, SDoWhile name body' cnd), next]
        _ -> E.throwError "mapWhile: No backreference here"

    addRef refs line key = M.adjust (second (line:)) key refs

    backrefs = L.foldl' (\refs (i, (mlbl, cmd)) ->
                          maybe id (`M.insert` (i, [])) mlbl $
                          maybe refs (addRef refs (i, cmd)) $ jumpLabel cmd
                        ) M.empty $ zip [0..] $ methodStmts m


replaceLabels labels = mapS go
  where
    getL            = flip M.lookup labels
    go (mlbl, stmt) = (mlbl, stmt')
      where
        stmt' = case stmt of
          SGoto   lbl -> fromMaybe stmt $ getL lbl
          SIf cnd lbl -> maybe stmt (\s -> SIfElse cnd [(Nothing, s)] []) $
                          getL lbl
          _            -> stmt


-- Switch statements
-- > lswitch v lblDef [(case0, lbl0), (case1, lbl1), (case2, lbl2)]
-- > lbl0: body0
-- > lbl1: body1
-- > lbl2: body2
-- > lblDef
-- > lblBreak
-- ==>
-- > switch v [(case0, body0), (case1, body1), (case2, body2))]
mapSwitch = mapRewrite $ do
  name <- (("switch_"++).show) <$> sourceLine <$> getPosition
  (lblStart, SLookupSwitch v lblDef cs0) <- switchP

  -- Group statements according to case
  let cs1 = map (first Just) cs0 ++ [(Nothing, lblDef)]
  cs2 <- mapM go $ zip cs1 $ tail cs1

  -- Build default block if another block breaks past it
  let ls0 = mapMaybe (jumpLabel.snd) $
            filter goto $ concatMap (mapA id.snd) cs2

  -- Find break-label if present
  let lblBreak = listToMaybe $ L.sort $ filter (>= lblDef) ls0

  -- Replace break-labels and collect default-case statements
  cs3 <- case lblBreak of
    Just lbl -> do
      -- Collect default-case if present
      defaultS <- if lbl > lblDef then upTo lbl else return []
      -- Replace break-labels and build new statement lists
      return $
        map (second (replaceLabels $ M.fromList [(lbl, SBreak name)])) $
        cs2 ++ [(Nothing, defaultS)]
    Nothing  -> return cs2

  -- Return rewritten statement
  return [(lblStart, SSwitch name v cs3)]
  where
    go ((n0, _), (n1, l1)) = do
      body <- upTo l1
      return (n0, body)

    upTo lbl = do
      body <- many labelLess
      stmt@(Just lbl0, s) <- label
      let body' = body ++ if s == SNop then [] else [stmt]
      if lbl0 == lbl then return body
        else ((body++).(stmt:)) <$> upTo lbl


db x = traceShow x x


mapTryCatch = mapRewrite $ do
  (mlbl1, STry sid lastTarget) <- tryP
  -- Parse try-body
  body1 <- readBody
  -- Parse catch cases (including finally)
  (catches, finally) <- partitionEithers <$> many1 (catch1 sid)
  -- Return tryCatch
  return [(mlbl1, STryCatch body1 catches $ listToMaybe finally)]

  where
    catch1 sid = try $ do
      (Nothing, SCatch sid' _ mexc) <- catchP
      guard $ sid' == sid
      body <- readBody
      return $ case mexc of
        Just exc -> Left  (exc, body)
        Nothing  -> Right body

    readBody = do
      ms <- optionMaybe $ satisfy $ not . catch_
      let stmt = return . maybeToList $ ms
      case ms of
        (Just (_, STry{}   )) -> E.throwError "Need to fix inner try first!"
        (Just (_, SGoto{}  )) -> return []
        (Just (_, SReturn{})) -> stmt
        (Just (_, SThrow{} )) -> stmt
        Nothing                -> stmt
        _                      -> liftM2 (++) stmt readBody

mapFixFinally = mapRewrite $ do
  (mlbl, s0@(STryCatch body0 catches0 (Just finally0))) <- anyStmt
  guard $ length finally0 > 2
  guard $ throw_ $ snd $ last finally0
  let
    -- Build cleanup function (skip exception pop and final throw)
    finally1 = drop 1 $ init finally0
    clean    = delFinally finally1
    -- Clean finally code from body and catches
    body1    = clean body0
    catches1 = map (second clean) catches0
    -- Clean recursively
    body2    = deep  clean body1
    catches2 = deep2 clean catches1
  return [(mlbl, STryCatch body2 catches2 $ Just finally1)]
  where
    throw_ SThrow{} = True
    throw_ _         = False

    delFinally _   []   = []
    delFinally fin body | s@(_, SThrow  _) <- last body = body
    delFinally fin body | fin == ending0 = prefix0
                        | fin == init ending1 = prefix1 ++ [last ending0]
                        | otherwise = body
      where
        (prefix1, ending1) = L.splitAt (length body - length fin - 1) body
        (prefix0, ending0) = L.splitAt (length body - length fin    ) body

    secondM f (a, b) = (a,) <$> f b

    deep2 clean = map (second $ mapS $ f clean)

    deep clean = mapS (f clean)

    f clean (mlbl, s) = (mlbl,) $ case s of
      STryCatch body catches mfinally ->
        STryCatch
          (dclean body)
          (map (second dclean) catches)
          (dclean `fmap` mfinally)
      SIfElse cnd left right -> SIfElse cnd (dclean left) (dclean right)
      SDoWhile nm body cnd -> SDoWhile nm (dclean body) cnd
      _ -> s
      where
        dclean = deep clean . clean

    -- fixBody f (lbl, s) = (lbl,) $ case s of
    --   STryCatch bd cs -> STryCatch (go bd) (go' cs)
    --   _                -> s
    --   where
    --     go   x = map (fixBody f) $ f x
    --     go' xs = map (second go) xs
