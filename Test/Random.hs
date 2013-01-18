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

import Test (runJavaTwice)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.FilePath
import System.Unix.Directory


type SGen v = W.WriterT [Stmt Value] (ST.StateT (M.Map Type [String]) Gen) v


lift2 = lift . lift


constG :: Type -> SGen Constant
constG t = case t of
  T_byte    -> C_int     <$> lift2 (choose (-127, 127))
  T_char    -> C_int     <$> lift2 (choose (-127, 127))
  T_int     -> C_int     <$> lift2 arbitrary
  T_boolean -> C_boolean <$> lift2 arbitrary
  T_float   -> C_float   <$> lift2 arbitrary
  T_double  -> C_double  <$> lift2 arbitrary


getG type_ = do
  reuse <- lift2 $ frequency [(10, return True), (1, return False)]
  mlst  <- ST.gets $ M.lookup type_
  case mlst of
    Just (l@(x:xs)) | reuse -> Local <$> (lift2 $ elements l)
    _                       -> create
  where
    prefix = "v_" ++ (drop 2 $ show type_)
    create = do
      lst <- ST.gets $ M.lookup type_
      case lst of
        Nothing -> ST.modify $ M.insert type_ [prefix ++ "0"]
        Just ls -> ST.modify $ M.insertWith (++) type_ [prefix ++ (show $ length ls)]
      name <- head <$> fromJust <$> (ST.gets $ M.lookup type_)
      val  <- VConst <$> constG type_
      W.tell [S_declare type_ (VarLocal $ Local name) val]
      return $ Local name


assignG :: SGen ()
assignG = do
  name <- getG T_int
  val  <- lift2 $ arbitrary
  W.tell [
    S_assign (VarLocal name) $ VConst $ C_int val]

binaryG :: SGen ()
binaryG = do
  a <- getG T_int
  b <- getG T_int
  r <- getG T_int
  op <- lift2 $ elements [E_add, E_sub, E_mul]
  W.tell [S_assign (VarLocal r) $ VExpr $
          op (VLocal $ VarLocal a) (VLocal $ VarLocal b)]


ifG :: SGen ()
ifG = do
  c <- getG T_boolean
  left  <- inline
  right <- inline
  W.tell [
    S_ifElse (E_eq (VLocal $ VarLocal c) (VConst $ C_boolean True)) left right]


inline = do
  st <- ST.get
  body <- lift2 $ sized $ \n -> flip ST.evalStateT st $
                                W.execWriterT $ stmtListG $ n `div` 2
  return $ map (Nothing,) body


printAll = do
  vs <- ST.gets M.elems
  W.tell $ map println $ L.sort $ concat vs

println v = S_assign (VarLocal $ Local "_")
            (VExpr $ E_invoke I_static
             (MethodSig (CF.Class "System.out") "println" []
              [T_object "java/lang/String"] T_void)
             [VLocal $ VarLocal $ Local v])


stmtG :: SGen ()
stmtG = do
  x <- lift2 $ frequency [(1, return True), (50, return False)]
  (lst, i) <- if x then
                (deeps,) <$> (lift2 $ elements [0..length deeps - 1])
              else
                (stmts,) <$> (lift2 $ elements [0..length stmts - 1])
  lst !! i
  where
    deeps = [ifG]
    stmts = [assignG, binaryG]

stmtListG :: Int -> SGen ()
stmtListG n = replicateM_ n stmtG >> printAll


data StmtList = StmtList { unStmtList :: [Stmt Value] }

instance Arbitrary StmtList where
  arbitrary = sized $ \n -> StmtList <$> (flip ST.evalStateT M.empty $
                                          W.execWriterT $ stmtListG n)


instance Arbitrary Java where
  arbitrary = join <$> map stmtToJava <$> unStmtList <$> arbitrary


data Code = Code String
            deriving Show

instance Arbitrary Code where
  arbitrary = do code <- flatCode <$> surround <$> (arbitrary :: Gen Java)
                 return $ Code $ code
    where
      surround (Java x) =
        Java [ JavaStmt 0 "package examples;"
             , JavaBlock "class TmpTestMain "
               [ JavaBlock "public static void main(String[] args) " x "" ]
               "" ]


randomTest = monadicIO $ do
  Code code <- pick arbitrary
  (stdout0, stdout1) <- run $ do
    writeFile ("examples" </> "TmpTestMain.java") code
    runJavaTwice $ "examples" </> "TmpTestMain"
  assert $ stdout0 == stdout1


test = do
  Code x <- head <$> drop 4 <$> sample' arbitrary
  putStrLn x