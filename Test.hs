{-# LANGUAGE OverloadedStrings, DeriveFoldable #-}

module Test where

import Prelude()
import CustomPrelude

-- HUnit
import Test.HUnit

-- Code generator
import Cogen
import Cogen.Java
import Cogen.Java.Jimple()

-- Parser
import qualified Parser as CF

import System.Exit
import System.Process
import System.Directory (copyFile, createDirectory)
import System.Unix.Directory (withTemporaryDirectory)

import Control.Monad.Error

import qualified Data.Text as T
import qualified Data.ByteString as B


decompileClass :: FilePath -> IO Text
decompileClass file = do
  cf <- CF.parseClassFile <$> B.readFile (pathString file)
  return $ flatCode $ toJava cf


runJavaOK :: MonadIO m => FilePath -> FilePath -> ErrorT String m String
runJavaOK workDir path = do
  -- Compile program
  _ <- run "javac" [pathString $ workDir </> path <.> "java"]
  run "java"  ["-cp", pathString workDir, pathString path]
  where
    run cmd args = do
      (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args ""
      when (exit /= ExitSuccess || not (null stderr)) $
        throwError $ "Command failed: " ++ stderr
      return stdout


runJavaTwice :: MonadIO m =>
                FilePath -> m (Either String String, Either String String)
runJavaTwice path = liftIO $ withTemporaryDirectory "jdd-test_" $ \tmpDir' -> do
  let tmpDir = stringPath tmpDir'
  makeDirs tmpDir $ splitDirectories $ directory path
  -- Test original
  copyFile (pathString javaPath) $ pathString (tmpDir </> javaPath)
  stdout0 <- run tmpDir
  -- Test decompiled
  code <- decompileClass $ tmpDir </> path <.> "class"
  writeFile (tmpDir </> javaPath) code
  stdout1 <- run tmpDir
  -- Return both stdouts
  return (stdout0, stdout1)
  where
    run workDir = liftIO $ runErrorT $ runJavaOK workDir path

    makeDirs dir (x:xs) | pathString x `elem` ["", "."] = makeDirs dir xs
    makeDirs dir (x:xs) = createDirectory (pathString $ dir </> x)
                          >> makeDirs (dir </> x) xs
    makeDirs dir []     = return ()

    javaPath = path <.> "java"


check assertEq fail (resultA, resultB) = do
  mapM_ (fail . show) $ lefts [resultA, resultB]
  let [stdoutA, stdoutB] = rights [resultA, resultB]
  assertEq "stdout" stdoutA stdoutB


testJava :: FilePath -> IO ()
testJava path = check assertEqual assertFailure =<< runJavaTwice path


makeTest :: Text -> Test
makeTest name = TestLabel (T.unpack name) $ TestCase $
                testJava $ "examples" </> textPath name


tests = runTestTT $ TestList $ map makeTest
        [ "HelloWorld"
        , "TIf"
        , "TDo"
        , "TDoWhile"
        , "TFor"
        , "TForEach"
        , "TField"
        , "TSwitch"
        , "TExcept"
        ]
