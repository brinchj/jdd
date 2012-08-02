{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Jimple.Maps where

import qualified Parser as CF

import Control.Monad
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

mapFix f v = fst $ head $ dropWhile (uncurry (/=)) $ zip l $ tail l
  where
    l = iterate f v

mapDecrypt (Method a b ops d) = Method a b (map go ops) d
  where
    go (l, S_invoke I_static sig [VConst (C_string x)] r)
      | methodClass  sig == CF.Class "dk/danid/plugins/Woddlecakes" &&
        methodName   sig == "int" &&
        methodParams sig == [T_object "java/lang/String"] =
          (l, S_assign r $ VConst $ C_string $ decrypt x)

    go s = s


decrypt = B.pack . snd . L.mapAccumL go 42 . delay
  where
    delay s = B.zip s $ B.drop 4 s

    go prev (k, k4) | isPrint $ chr k4i = (dec,  fromIntegral dec)
                    | otherwise         = (prev, k4)
      where
        (ki, k4i) = (fromIntegral k, fromIntegral k4)
        dec       = ((prev + ki + k4i) `mod` 95) + 32


pureValue (VConst _) = True
pureValue (VLocal (VarLocal _)) = True
pureValue _ = False


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