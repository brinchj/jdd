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


decompileClass :: FilePath -> IO String
decompileClass file = do
  cf <- CF.parseClassFile <$> B.readFile file
  return $ flatCode $ toJava cf