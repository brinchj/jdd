{-# LANGUAGE OverloadedStrings, DeriveFoldable #-}

module Test where

import Data.Foldable hiding (mapM_)

import Jimple
import Jimple.Typing
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
  let (err, meth) = parseJimple cf method
      mapSimple = mapDecrypt . mapCleanup . mapInline
      mapsF = mapGotoIf . mapElimGoto . mapFix mapSimple . mapCorrectLabels
      meth'@(Method a b c d) = mapsF meth
  mapM_ (print) c
  maybe (return ()) print err

  return meth'