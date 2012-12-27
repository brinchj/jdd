{-# LANGUAGE OverloadedStrings, DeriveFoldable #-}

module Test where

import Data.Foldable hiding (mapM_)

import Jimple
import Jimple.Typing
import Jimple.Types
import Jimple.Maps

-- import Cogen
import Cogen
import Cogen.Java
import Cogen.Java.Jimple


import qualified Parser as CF

import qualified Data.Map as Map

import Control.Applicative
import qualified Data.ByteString as B


list path =
  Map.keys . CF.classMethods . CF.parseClassFile <$> B.readFile path

phase1 = mapCorrectLabels
phase2 = mapFix $ mapCleanup . mapInline
phase3 = mapFix $ mapSwitch . mapWhile . mapGotoIf . mapElimGoto

run path method = do
  cf <- CF.parseClassFile <$> B.readFile path
  print cf

  let (err, meth0) = parseJimple cf method
      -- Code rewriting
      transform = phase3 . phase2 . phase1
      meth1 = transform meth0
      -- Type local declarations
      meth2 = simpleTyper meth1
      -- Unpack
      Method sig ls idents code excs = meth2

  maybe (return ()) print err

  print ls
  print idents

  putStrLn "\n--\nMethod code:"
  putStrLn $ flatCode $ join $ map toJava $ map snd code
  putStrLn "--\n"

  return meth2