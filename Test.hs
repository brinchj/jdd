{-# LANGUAGE OverloadedStrings, DeriveFoldable #-}

module Test where

-- HUnit
import Test.HUnit

-- Jimple
import Jimple
import Jimple.Typing
import Jimple.Types
import Jimple.Maps

-- Code generator
import Cogen
import Cogen.Java
import Cogen.Java.Jimple

-- Parser
import qualified Parser as CF

import System.Exit
import System.Process
import System.Directory
import System.Unix.Directory
import System.FilePath

import Control.Monad.Error
import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Either


decompileClass :: FilePath -> IO String
decompileClass file = do
  cf <- CF.parseClassFile <$> B.readFile file
  return $ flatCode $ toJava cf


runJavaOK :: MonadIO m => FilePath -> FilePath -> ErrorT String m String
runJavaOK workDir path = do
  -- Compile program
  run "javac" [workDir </> path <.> "java"]
  run "java"  ["-cp", workDir, path]
  where
    run cmd args = do
      (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args ""
      when (exit /= ExitSuccess || not (null stderr)) $
        throwError $ "Command failed: " ++ stderr
      return stdout


runJavaTwice :: MonadIO m =>
                FilePath -> m (Either String String, Either String String)
runJavaTwice path = liftIO $ withTemporaryDirectory "jdd-test" $ \tmpDir -> do
  makeDirs tmpDir $ splitDirectories $ takeDirectory path
  -- Test original
  copyFile javaPath $ tmpDir </> javaPath
  stdout0 <- run tmpDir
  -- Test decompiled
  code <- decompileClass $ tmpDir </> path <.> "class"
  writeFile (tmpDir </> javaPath) code
  stdout1 <- run tmpDir
  -- Return both stdouts
  return (stdout0, stdout1)
  where
    run workDir = liftIO $ runErrorT $ runJavaOK workDir path

    makeDirs dir ("/":xs) = makeDirs dir xs
    makeDirs dir (".":xs) = makeDirs dir xs
    makeDirs dir (x:xs) = createDirectory (dir </> x) >> makeDirs (dir </> x) xs
    makeDirs dir []     = return ()

    javaPath = path <.> "java"


check assertEq fail (resultA, resultB) = do
  mapM_ (fail.show) $ lefts [resultA, resultB]
  let [stdoutA, stdoutB] = rights [resultA, resultB]
  assertEq "stdout" stdoutA stdoutB


testJava :: FilePath -> IO ()
testJava path = check assertEqual assertFailure =<< runJavaTwice path


makeTest name = TestLabel name $ TestCase $ testJava $ "examples" </> name


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
