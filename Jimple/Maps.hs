{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Jimple.Maps where

import qualified Parser as CF

import Control.Monad
import Control.Arrow (second)

import qualified Control.Monad.State as ST
import qualified Control.Monad.Writer as W

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char
import Data.Maybe
import Data.Word

import Jimple.Types
import Jimple.Rewrite


-- Apply map until a fix-point is reached
mapFix f v = fst $ head $ dropWhile (uncurry (/=)) $ zip l $ tail l
  where
    l = iterate f v


-- Replace calls to Woddlecakes.int with decrypted string-constants
mapDecrypt (Method a b ops d) = Method a b (map go ops) d
  where
    go (l, S_assign r (VExpr (E_invoke I_static sig [VConst (C_string x)])))
      | methodClass  sig == CF.Class "dk/danid/plugins/Woddlecakes" &&
        methodName   sig == "int" &&
        methodParams sig == [T_object "java/lang/String"] =
          (l, S_assign r $ VConst $ C_string $ decrypt x)

    go s = s


-- Decrypt a Woddlecakes-encrypted string
decrypt = B.pack . snd . L.mapAccumL go 42 . delay
  where
    delay s = B.zip s $ B.drop 4 s

    go prev (k, k4) | isPrint $ chr k4i = (dec,  fromIntegral dec)
                    | otherwise         = (prev, k4)
      where
        (ki, k4i) = (fromIntegral k, fromIntegral k4)
        dec       = ((prev + ki + k4i) `mod` 95) + 32


-- Identify pure values (no side-effects, used for inlining and cleaning)
pureValue (VConst _) = True
pureValue (VLocal (VarLocal _)) = True
pureValue _ = False


-- Clean up dead code by removing pure values that aren't used
mapCleanup (Method a b ops d) = Method a b (go ops) d
  where
    go s = reverse $ catMaybes $ ST.evalState (mapM go' $ reverse s) S.empty

    go' (l@(label, s@(S_assign (VarLocal v) e))) = do
      alive <- ST.gets $ S.member v
      when alive $ addAlive s
      let canRemove = not alive && pureValue e
      case label of
        Nothing | canRemove -> return Nothing
        Just _  | canRemove -> return $ Just (label, S_nop)
        _ -> return $ Just l

    go' (l@(_, st)) = addAlive st >> return (Just l)

    addAlive = ST.modify . S.union . F.foldl f S.empty
      where
        f s (VLocal (VarLocal v)) = S.insert v s
        f s (VExpr  e) = F.foldl f s e
        f s _ = s


-- Perform value-inlining of pure values
mapInline (Method a b ops d) = Method a b (go ops) d
  where
    go s = ST.evalState (mapM go' s) M.empty

    go' (l, s) = do
      m <- ST.get
      update s
      return (l, inline m `fmap` s)

    inline m (val@(VLocal (var@(VarLocal (Local ('s':_)))))) =
      fromMaybe val $ M.lookup var m
    inline m (VExpr e)  = VExpr $ inline m `fmap` e
    inline m e = e

    update (S_assign v e) | pureValue e = ST.modify $ M.insert v e
    update _                            = return ()


-- Rewrite labels from relative to absolute, while removing unsed ones.
mapCorrectLabels (Method a b ops d) = Method a b (go ops) d
  where
    go s = map f s'
      where
       (s', labels) = W.runWriter $ mapM go' s

       f (Just l, s) | l `elem` labels = (Just l,  s)
                     | otherwise       = (Nothing, s)

    go' (Just pos, S_if c next) = tellLabel (S_if c) pos next
    go' (Just pos, S_goto next) = tellLabel S_goto   pos next

    -- TODO: S_lookupSwitch, S_tableSwitch
    go' s = return s


    tellLabel f (Label pos) (Label next) = do
      W.tell [Label $! pos + next]
      return (Just $ Label pos, f $ Label $ pos + next)


-- Rewrite Jimple code according to rule
mapRewrite rule (Method a b ops d) = Method a b (go ops) d
  where
    go ops = maybe ops go $ rewrite rule ops



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

  -- compute line size of parsed statements
  let size = 5 + length (body1 ++ body2)
  -- rearrange blocks without gotos
  let body = (body2Top:body2) ++ (body1Top:body1)

  return (size, body)


-- Try to clean up if-statements:
-- > ifLbl: if cond lbl1
-- > ... body1
-- { > lbl1: } ==> if cond [] body1
-- { > goto lbl2
--   > lbl1:
--   > ... body2
--   > lbl2: } ==> if cond body2 body1
mapGotoIf = mapRewrite $ do
  (ifLbl, S_if cond lbl1) <- satisfy if_
  body1 <- many jumpless
  next <- anyStmt
  let sizeIf = 1 + length body1
  case next of
    (Just lbl1', _) | lbl1 == lbl1' ->
      -- if with no else
      return (sizeIf, [(ifLbl, S_ifElse cond [] body1)])
    (_, S_goto lbl2) -> do
      -- if with else part: if cond 1 ... goto 2, 1: ... 2:
      body2Top@(Just lbl1', _) <- anyStmt
      guard $ lbl1 == lbl1'
      body2 <- many jumpless
      -- parse end of if (not counted)
      (Just lbl2', _) <- anyStmt
      guard $ lbl2 == lbl2'
      -- all ok!
      let sizeElse = sizeIf + 2 + length body2
      return (sizeElse, [(ifLbl, S_ifElse cond body2 body1)])
    _ -> fail "mapGotoIf: no match"



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
mapWhile  (Method a b ops d) = Method a b (go ops) d
  where
    go ops = fromMaybe ops $ rewrite rule ops

    rule = do
      -- error $ show backrefs
      (Just lblStart, stmtStart) <- anyStmt
      let name = show lblStart
      case M.lookup lblStart backrefs of
        Just (i, (j, stmt'):rs) | i < j -> do
          -- Extract loop body and final loop-jump
          body <- replicateM (j - i - 1) anyStmt
          (lblEnd, stmtEnd)  <- jumpP
          -- Sanity check
          unless (jumpLabel stmtEnd == Just lblStart) $
            fail "mapWhile: Mismatch between labels"
          (lblNext, stmtNext) <- anyStmt
          -- Setup break/continue statements
          let labels = maybe id (`M.insert` S_break name) lblNext $
                       M.fromList [(lblStart, S_continue name :: Stmt Value)]
          -- Rewrite labels inside body
          let body' = replaceLabels labels `map` body
          let cnd = case stmtEnd of
                S_goto _    -> VConst $ C_boolean True
                S_if cnd' _ -> VExpr cnd'
          -- Construct doWhile
          return (j - i + 1, [(Nothing, S_doWhile name body' cnd)])
        _ -> fail "mapWhile: No backreference here"

    addRef refs line key = M.adjust (second (line:)) key refs

    backrefs = L.foldl' (\refs (i, (mlbl, cmd)) ->
                          maybe id (`M.insert` (i, [])) mlbl $
                          maybe refs (addRef refs (i, cmd)) $ jumpLabel cmd
                        ) M.empty $ zip [0..] ops

    replaceLabels labels (mlbl, stmt) = (mlbl, stmt')
      where
        go    = map (replaceLabels labels)
        getL  = flip M.lookup labels
        stmt' = case stmt of
          (S_ifElse cnd left right) -> S_ifElse cnd (go left) (go right)
          (S_goto   lbl) -> fromMaybe stmt $ getL lbl
          (S_if cnd lbl) -> maybe stmt (\s -> S_ifElse cnd [(Nothing, s)] []) $
                            getL lbl
          _            -> stmt :: Stmt Value