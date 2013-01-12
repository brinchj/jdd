module Main where

import System.Environment

import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Parser as CF

import Cogen             (flatCode)
import Cogen.Java        (toJava)
import Cogen.Java.Jimple ()

import Test (tests, makeTest)
import Test.HUnit (runTestTT, Test(..))


usage name = putStrLn $ concat [name, " [-|classfile|--test|--help]" ]

main = do
  name <- getProgName
  args <- getArgs
  let (cmd, rest) = if null args then ("", []) else (head args, tail args)
  case cmd of
    "" -> usage name
    "--help" -> usage name
    "--test" | null rest -> void $ tests
             | otherwise -> void $ runTestTT $ TestList $ map makeTest rest

    file -> do
      bytes <- if file == "-" then B.getContents else B.readFile file
      putStrLn $ flatCode $ toJava $ CF.parseClassFile bytes


