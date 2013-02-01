{-# LANGUAGE OverloadedStrings #-}

module Cogen
       ( Code
       , CodeRows
       , Codeable(..)
       , flatCode
       )
       where

import Prelude()
import CustomPrelude

import qualified Data.Text as T


type Code        = Text
type CodeRows    = [[Text]]
type Indentation = Int


-- A value is Codeable when it can be transformed into a list of line-pieces
class Codeable a where
  toCode :: Indentation -> a -> CodeRows


-- Convert our internal representation of code rows to regular "flat" code
flatCode :: Codeable a => a -> Code
flatCode stmt = T.intercalate "\n" $ map concat rows
  where
    rows = toCode 0 stmt