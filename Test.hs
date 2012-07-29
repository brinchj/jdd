{-# LANGUAGE OverloadedStrings, DeriveFoldable #-}

import Data.Foldable hiding (mapM_)

import Jimple
import Jimple.Types
import Jimple.Maps

import qualified Parser as CF

import qualified Data.Map as Map

import Control.Applicative
import qualified Data.ByteString as B


list path =
  Map.keys . CF.classMethods . CF.parseClassFile <$> B.readFile path

run path method = do
  cf <- CF.parseClassFile <$> B.readFile path
  print cf
  let m = CF.classMethods cf Map.! method
  let code = CF.blockAttrs m Map.! "Code"
  print m
  let (err, meth) = parseJimple cf code
      maps = mapDecrypt . mapCleanup . mapInline
      meth'@(Method a b c d) = mapFix maps meth
  mapM_ (print . snd) c
  maybe (return ()) print err

  return meth'