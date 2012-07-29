{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Jimple.Maps where

import qualified Parser as CF

import Control.Monad
import qualified Control.Monad.State as ST

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char
import Data.Maybe

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


decrypt = B.pack . conv . go 42 . delay . conv . B.unpack
  where
    delay s = zip s $ drop 4 s

    conv :: (Integral a, Num b) => [a] -> [b]
    conv = map fromIntegral

    go :: Int -> [(Int, Int)] -> [Int]
    go _ [] = []
    go p ((k, k4):ks) | k4 >= 32 && k4 <= 128 = dec : go dec ks
                      | otherwise             =  k4 : go   p ks
      where
        dec = ((p + k + k4) `mod` 95) + 32


pureValue (VConst _) = True
pureValue (VLocal (VarLocal _)) = True
pureValue _ = False


mapCleanup (Method a b ops d) = Method a b (go ops) d
  where
    go s = reverse $ catMaybes $ ST.evalState (mapM go' $ reverse s) S.empty

    go' (l@(_, s@(S_assign (VarLocal v) e))) = do
      alive <- ST.gets $ S.member v
      when alive $ addAlive s
      return $ if (pureValue e && not alive)
               then Nothing
               else Just l

    go' (l@(_, st)) = addAlive st >> (return $ Just l)

    addAlive st = ST.modify $ S.union $ F.foldl f S.empty st
      where
        f s (VLocal (VarLocal v)) = S.insert v s
        f s (VExpr  e) = F.foldl f s e
        f s (VConst c) = s


mapInline (Method a b ops d) = Method a b (go ops) d
  where
    go s = ST.evalState (mapM go' s) M.empty

    go' (l, s) = do
      (m :: M.Map (Variable Value) Value) <- ST.get
      update s
      return (l, inline m `fmap` s)

    inline m (val@(VLocal (var@(VarLocal l)))) = maybe val id $ M.lookup var m
    inline m (c@(VConst _)) = c
    inline m (VExpr e)  = VExpr $ inline m `fmap` e

    update (S_assign v e) | pureValue e = ST.modify $ M.insert v e
    update _ = return ()