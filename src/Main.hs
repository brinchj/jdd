module Main where

import Prelude()
import CustomPrelude

import System.Environment (getProgName)
import System.Exit (exitFailure)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Parser as CF

import Cogen             (flatCode)
import Cogen.Java        (toJava)
import Cogen.Java.Jimple ()

import Test (tests, makeTest)
import Test.Random (randomTest)

import Test.HUnit (runTestTT, Test(..))
import Test.QuickCheck (verboseCheck)


usage name = putStrLn $ name ++ " [-|classfile|--test|--test-qc|--help]"

main = do
  name <- fromString <$> getProgName
  args <- getArgs
  let (cmd, rest) = if null args then ("", []) else (head args, tail args)
  case cmd of
    "" -> usage name
    "--help" -> usage name
    "--test" | null rest -> void tests
             | otherwise -> void $ runTestTT $ TestList $
                            map makeTest rest

    "--test-qc" -> verboseCheck randomTest
                      -- putStrLn "\n\nOutput:"
                      -- putStrLn $ output r
                      -- putStrLn $ "\n\nReason:" ++ reason r

    _ | "--" <- T.take 2 cmd -> do
      usage name
      putStrLn $ "Error: Unknown flag " ++ cmd
      exitFailure

    fileOrStdin -> do
      bytes <- if fileOrStdin == "-" then B.getContents
               else B.readFile $ fromText fileOrStdin
      putStrLn $ flatCode $ toJava $ CF.parseClassFile bytes


