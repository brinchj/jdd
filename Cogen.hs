module Cogen
       ( Code
       , CodeRows
       , Codeable(..)
       , flatCode
       )
       where

import Data.List

type Code        = String
type CodeRows    = [[String]]
type Indentation = Int

-- A value is Codeable when it can be transformed into a list of line-pieces
class Codeable a where
  toCode :: Indentation -> a -> CodeRows


-- Convert our internal representation of code rows to regular "flat" code
flatCode :: Codeable a => a -> Code
flatCode stmt = intercalate "\n" $ map concat rows
  where
    rows = toCode 0 stmt