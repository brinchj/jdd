{-# LANGUAGE OverloadedStrings #-}

module Jimple.Maps where

import qualified Data.ByteString as B
import Data.Char

import Jimple.Types
import qualified Parser as CF

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

