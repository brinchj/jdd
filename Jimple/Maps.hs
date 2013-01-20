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
mapS f ls = map go ls
  where
    go (mlbl, stmt) = f (mlbl, stmt')
      where
        stmt' = case stmt of
          S_ifElse  cnd  l r  -> S_ifElse  cnd    (map go l) (map go r)
          S_doWhile name b c  -> S_doWhile name   (map go b) c
          S_switch  name v cs -> S_switch  name v [(c, map go s) | (c, s) <- cs]
          _                   -> stmt

-- Like mapS but returns values of f isntead of replacing inside the structure
mapA f ls = map f $ foldS (flip (:)) [] ls

-- Fold through statements recursively
foldS f zero ls = F.foldl' go zero ls
  where
    go acc0 (mlbl, stmt) = acc2
      where
        acc1 = f acc0 (mlbl, stmt)
        acc2 = case stmt of
          S_ifElse  _ l r  -> F.foldl' go (F.foldl' go acc1 l) r
          S_doWhile _ b _  -> F.foldl' go acc1 b
          S_switch  _ _ cs -> F.foldl' go acc1 $ L.concat $ map snd cs
          _                -> acc1


-- Apply map until a fix-point is reached
mapFix f v = fst $ head $ dropWhile (uncurry (/=)) $ zip l $ tail l
  where
    l = iterate f v


-- StringBuilder.append("") does nothing
mapAppendEmpty m = m { methodStmts = map go $ methodStmts m }
  where
    go (l, S_assign r (VExpr (E_invoke (I_virtual v) sig [VConst (C_string "")])))
      | methodClass  sig == CF.Class "java/lang/StringBuilder" &&
        methodName   sig == "append" &&
        methodParams sig == [T_object "java/lang/String"] =
          (l, S_assign r v)

    go s = s

-- Identify pure values (no side-effects, used for inlining and cleaning)
pureValue (VExpr (E_invoke _ _ _)) = False
pureValue (VExpr (E_new r _args)) = case r of
  R_instanceField _ _ -> True
  R_staticField   _ _ -> True
  _ -> False
pureValue _ = True


-- Clean up dead code by removing pure values that aren't used
mapCleanup m = m { methodStmts = go $ methodStmts m }
  where
    go s = reverse $ catMaybes $ ST.evalState (mapM go' $ reverse s) S.empty

    go' (l@(label, s@(S_assign (VarLocal v@(Local ('s':_))) e))) = do
      alive <- ST.gets $ S.member v
      when alive $ addAlive s
      let canRemove = not alive && pureValue e
      case label of
        Nothing | canRemove -> return Nothing
        Just _  | canRemove -> return $ Just (label, S_nop)
        _ -> return $ Just l

    -- remove dead lines
    go' (Nothing, S_nop) = return Nothing

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

    update (S_assign v e) | pureValue e = ST.modify $ M.insert v e
    update _                            = return ()


-- Rewrite labels from relative to absolute, while removing unused ones.
mapCorrectLabels m = m { methodStmts = go $ methodStmts m }
  where
    go s = map f s'
      where
       (s', labels) = W.runWriter $ mapM go' s

       f (Just l, s) | l `elem` labels = (Just l,  s)
       f (_     , s)                   = (Nothing, s)

    go' (Just pos, S_if c next) = tellLabel (S_if c) pos next
    go' (Just pos, S_goto next) = tellLabel S_goto   pos next
    go' (Just pos, S_lookupSwitch v lbl cs) = do
      let cs'  = map (second (+pos)) cs
      let lbl' = pos + lbl
      W.tell $ lbl' : map snd cs'
      return (Just pos, S_lookupSwitch v lbl' cs')

    -- TODO: S_tableSwitch
    go' s = return s


    tellLabel f (Label pos) (Label next) = do
      W.tell [Label $! pos + next]
      return (Just $ Label pos, f $ Label $ pos + next)


-- Move argument to init-calls into their respective new call (constructor)
mapCorrectInit m = m { methodStmts = go $ methodStmts m }
  where
    go s = catMaybes $ ST.evalState (mapM goM s) M.empty

    goM stmt@(lbl, s) = case s of
      S_assign v (VExpr (E_new (R_object cl) [])) -> do
        ST.modify $ M.insert cl v
        return $ Nothing

      S_assign _ (VExpr
                  (E_invoke
                   (I_special (VLocal v))
                   (MethodSig cl "<init>" _fs _ts _rt) pars)) -> do
        realv <- fromMaybe v <$> (ST.gets $ M.lookup cl)
        return $ Just (lbl, S_assign realv (VExpr (E_new (R_object cl) pars)))

      _ -> return $ Just stmt



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
  S_goto lbl2 <- gotoP
  body1Top@(Just lbl1, _) <- label

  body1 <- many jumpless

  S_goto lbl3 <- gotoP
  body2Top@(Just lbl2', _) <- label
  guard $ lbl2 == lbl2'

  body2 <- many jumpless

  S_goto lbl1' <- gotoP
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
  (ifLbl, S_if cond lbl1) <- ifP
  body1 <- bodyM lbl1
  next  <- anyStmt
  case next of
    -- if with else part: if cond 1 ... goto 2, 1: ... 2:
    (_, S_goto lbl2) -> do
      body2 <- bodyM lbl2
      return [(ifLbl, S_ifElse cond body2 body1)]

    -- if with no else
    (Just lbl', _) | lbl' == lbl1 ->
      return [(ifLbl, S_ifElse cond [] body1), next]

    _ -> E.throwError "mapGotoIf: mismatch"

  where
    bodyS lbl (_, S_goto _) = False
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
          let labels = maybe id (`M.insert` S_break name) lblNext $
                       M.fromList [(lblStart, S_continue name :: Stmt Value)]
          -- Rewrite labels inside body
          let body' = (Nothing, stmtStart) : replaceLabels labels body
          let cnd = case stmtEnd of
                S_goto _    -> VConst $ C_boolean True
                S_if cnd' _ -> VExpr cnd'
          -- Construct doWhile
          return [(Just lblStart, S_doWhile name body' cnd), next]
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
          S_goto   lbl -> fromMaybe stmt $ getL lbl
          S_if cnd lbl -> maybe stmt (\s -> S_ifElse cnd [(Nothing, s)] []) $
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
  (lblStart, S_lookupSwitch v lblDef cs0) <- switchP

  -- Group statements according to case
  let cs1 = map (first Just) cs0 ++ [(Nothing, lblDef)]
  cs2 <- mapM go $ zip cs1 $ tail cs1

  -- Build default block if another block breaks past it
  let ls0 = catMaybes   $ map (jumpLabel.snd) $
            filter goto $ L.concat $ map (mapA id.snd) cs2

  -- Find break-label if present
  let lblBreak = listToMaybe $ L.sort $ filter (>= lblDef) ls0

  -- Replace break-labels and collect default-case statements
  cs3 <- case lblBreak of
    Just lbl -> do
      -- Collect default-case if present
      defaultS <- if lbl > lblDef then upTo lbl else return []
      -- Replace break-labels and build new statement lists
      return $
        map (second (replaceLabels $ M.fromList [(lbl, S_break name)])) $
        cs2 ++ [(Nothing, defaultS)]
    Nothing  -> return cs2

  -- Return rewritten statement
  return [(lblStart, S_switch name v cs3)]
  where
    go ((n0, _), (n1, l1)) = do
      body <- upTo l1
      return (n0, body)

    upTo lbl = do
      body <- many labelLess
      stmt@(Just lbl0, s) <- label
      let body' = body ++ if s == S_nop then [] else [stmt]
      if | lbl0 /= lbl -> ((body++).(stmt:)) <$> upTo lbl
         | otherwise   -> return body'


db x = traceShow x x


mapTryCatch = mapRewrite $ do
  (mlbl1, S_try sid lastTarget) <- tryP
  -- Parse try-body
  body1 <- readBody
  -- Parse catch cases (including finally)
  (catches, finally) <- partitionEithers <$> many1 (catch1 sid)
  -- Return tryCatch
  return [(mlbl1, S_tryCatch body1 catches $ listToMaybe finally)]

  where
    catch1 sid = try $ do
      (Nothing, S_catch sid' _ mexc) <- catchP
      guard $ sid' == sid
      body <- readBody
      return $ case mexc of
        Just exc -> Left  (exc, body)
        Nothing  -> Right body

    readBody = do
      ms <- optionMaybe $ satisfy $ not . catch_
      let stmt = maybe (return []) (return.(:[])) ms
      case ms of
        (Just (_, S_try{}   )) -> E.throwError "Need to fix inner try first!"
        (Just (_, S_goto{}  )) -> return []
        (Just (_, S_return{})) -> stmt
        (Just (_, S_throw{} )) -> stmt
        Nothing                -> stmt
        _                      -> liftM2 (++) stmt readBody

mapFixFinally = mapRewrite $ do
  (mlbl, S_tryCatch body catches finally) <- anyStmt
  fail ""
  where
    fixFinally body1 cs0 finBody1 =
        let finBody2 = drop 1 $ init finBody1
            delFin   = delFinally finBody2
            body2    = delFin body1
            cs2      = [ (exc, delFin bd) | (exc, bd) <- cs0 ]
        in
        (body2, cs2 ++ [(Nothing, finBody2)])

    delFinally fin body | s@(_, S_throw  _) <- last body = body
    delFinally fin body | fin == ending      = prefix
                        | fin == init ending = prefix ++ [last ending]
                        | otherwise = body
      where
        ending = drop (length prefix) body
        prefix = take (length body - length fin) body

    -- fixBody f (lbl, s) = (lbl,) $ case s of
    --   S_tryCatch bd cs -> S_tryCatch (go bd) (go' cs)
    --   _                -> s
    --   where
    --     go   x = map (fixBody f) $ f x
    --     go' xs = map (second go) xs
