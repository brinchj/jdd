{-# LANGUAGE FlexibleInstances
           , TupleSections
           , OverloadedStrings
  #-}

module Test.Random where

import qualified Data.ByteString.Char8 as B

import qualified Parser as CF
import Jimple.Types

import Cogen (flatCode)
import Cogen.Java
import Cogen.Java.Jimple (stmtToJava)

import Control.Applicative
import Control.Monad hiding (join)
import Control.Monad.Trans

import qualified Control.Monad.State  as ST
import qualified Control.Monad.Writer as W

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map  as M

import Test (runJavaTwice, check)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.FilePath
import System.Unix.Directory


type SGen v = W.WriterT [Stmt Value] (ST.StateT (M.Map Type [String]) Gen) v


lift2 = lift . lift


constG :: Type -> SGen Constant
constG t = case t of
  TByte    -> CInt     <$> lift2 (choose (-127, 127))
  TChar    -> CInt     <$> lift2 (choose (-127, 127))
  TInt     -> CInt     <$> lift2 arbitrary
  TBoolean -> CBoolean <$> lift2 arbitrary
  TFloat   -> CFloat   <$> lift2 arbitrary
  TDouble  -> CDouble  <$> lift2 arbitrary


getG type_ = do
  reuse <- lift2 $ frequency [(10, return True), (1, return False)]
  mlst  <- ST.gets $ M.lookup type_
  case mlst of
    Just (l@(x:xs)) | reuse -> Local <$> lift2 (elements l)
    _                       -> create
  where
    prefix = 'v' : drop 1 (show type_)
    create = do
      lst <- ST.gets $ M.lookup type_
      case lst of
        Nothing -> ST.modify $ M.insert type_ [prefix ++ "0"]
        Just ls -> ST.modify $
                   M.insertWith (++) type_ [prefix ++ show (length ls)]
      name <- head <$> fromJust <$> ST.gets (M.lookup type_)
      val  <- VConst <$> constG type_
      W.tell [SDeclare type_ (VarLocal $ Local name) val]
      return $ Local name


assignG :: SGen ()
assignG = do
  name <- getG TInt
  val  <- lift2 arbitrary
  W.tell [
    SAssign (VarLocal name) $ VConst $ CInt val]

binaryG :: SGen ()
binaryG = do
  a <- getG TInt
  b <- getG TInt
  r <- getG TInt
  op <- lift2 $ elements [EAdd, ESub, EMul]
  W.tell [SAssign (VarLocal r) $ VExpr $
          op (VLocal $ VarLocal a) (VLocal $ VarLocal b)]


ifG :: SGen ()
ifG = do
  c <- getG TBoolean
  left  <- inline
  right <- inline
  W.tell [
    SIfElse (EEq (VLocal $ VarLocal c) (VConst $ CBoolean True)) left right]


inline = do
  st <- ST.get
  body <- lift2 $ sized $ \n -> flip ST.evalStateT st $
                                W.execWriterT $ stmtListG $ n `div` 2
  return $ map (Nothing,) body


printAll = do
  vs <- ST.gets M.elems
  mapM_ W.tell $ map println $ L.sort $ concat vs

println v | "Boolean" `L.isInfixOf` v = [
  SIfElse (EEq (VLocal $ VarLocal $ Local v) (VConst $ CBoolean True))
  [(Nothing, out $ VConst $ CString "true" )]
  [(Nothing, out $ VConst $ CString "false")]
  ]
          | otherwise                 = [out $ VLocal $ VarLocal $ Local v]
  where
    out a =
      SAssign (VarLocal $ Local "_")
      (VExpr $ EInvoke IStatic
       (MethodSig (CF.Class "System.out") "println" []
        [TObject "java/lang/String"] TVoid) [a])


stmtG :: SGen ()
stmtG = do
  x <- lift2 $ frequency [(1, return True), (50, return False)]
  let lst = if x then deeps else stmts
  i <- lift2 $ choose (0, length lst - 1)
  lst !! i
  where
    deeps = [ifG]
    stmts = [assignG, binaryG]

stmtListG :: Int -> SGen ()
stmtListG n = replicateM_ n stmtG >> printAll


data StmtList = StmtList { unStmtList :: [Stmt Value] }

instance Arbitrary StmtList where
  arbitrary = sized $ \n -> StmtList <$> ST.evalStateT
                            (W.execWriterT $ stmtListG n) M.empty


instance Arbitrary Java where
  arbitrary = join <$> map stmtToJava <$> unStmtList <$> arbitrary


data Code = Code String

instance Show Code where
  show (Code s) = s

instance Arbitrary Code where
  arbitrary = do code <- flatCode <$> surround <$> (arbitrary :: Gen Java)
                 return $ Code code
    where
      surround (Java x) =
        Java [ JavaStmt 0 "package examples;"
             , JavaBlock "class TmpTestMain "
               [ JavaBlock "public static void main(String[] args) " x "" ]
               "" ]


randomTest = monadicIO $ do
  Code code <- pick arbitrary
  result <- run $ do
    writeFile ("examples" </> "TmpTestMain.java") code
    runJavaTwice $ "examples" </> "TmpTestMain"
  check assertEq fail result
  where
    assertEq name a b | a == b    = return ()
                      | otherwise = fail msg
      where
        msg = name ++ ": expected:\n" ++ a ++ "\nbut got:\n" ++ b


test = do
  Code x <- head <$> drop 4 <$> sample' arbitrary
  putStrLn x