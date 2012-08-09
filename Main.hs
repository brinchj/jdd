{--- # LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad
import System.Environment
-- import Test

import qualified Data.ByteString.Char8 as B


main = do
  cmd:args <- getArgs
  case cmd of
    "list" -> print =<< B.readFile a1
      where [a1] = args

    -- "run" -> print (a1, a2)
    --   where [a1, a2] = args

