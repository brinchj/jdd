module Cogen.Java where

import Prelude()
import CustomPrelude

import Cogen

import qualified Data.Text as T


-- We can construct Java-code from a list of Java statements
data Java = Java [JavaStmt]
          deriving Show

instance Codeable Java where
  toCode ind (Java stmts) = concatMap (toCode ind) stmts


-- Data types to represent blocks of Java-statements
data JavaStmt = JavaBlock Text [JavaStmt] Text
              | JavaStmt  { javaIndent :: Int
                          , javaStmt   :: Text }
              deriving Show

-- Blocks of Java-statements can be used to construct Java-code
instance Codeable JavaStmt where
  toCode ind (JavaStmt relInd stmt) =
    [[T.replicate (ind + relInd) " ", stmt]]

  toCode ind (JavaBlock left stmts right) =
    let indS = T.replicate ind " " in
    [indS, left, "{"] : concatMap (toCode $ 4 + ind) stmts ++
    [ [indS, "}", right ] ]



-- A typeclass for things we can turn into Java
class Javable a where
  toJava :: a -> Java

instance Javable a => Javable [a] where
  toJava l = concat $ map toJava l

instance Javable Java where
  toJava = id

instance Javable JavaStmt where
  toJava = Java . return


instance Monoid Java where
  mempty = Java []
  mappend (Java a) (Java b) = Java $ a ++ b
  mconcat = foldl' (++) mempty


-- A simple example
example1 = flatCode $
           Java [ JavaStmt 0 "int foo()"
                , JavaBlock "" [ JavaStmt 0 "int a = 0;"
                               , JavaBlock "while (a < 10) " [
                                   JavaStmt 0 "a += 1;"
                                   ] ""
                               , JavaBlock "do " [
                                 JavaStmt 0 "a += 1;"
                                 ] " while (a < 20) ;"
                               , JavaStmt 0 "return a;"
                               ] ""
                ]

-- *Cogen.Java> putStrLn example1
-- int foo()
-- {
--     int a = 0;
--     while (a < 10) {
--         a += 1;
--     }
--     do {
--         a += 1;
--     } while (a < 20) ;
--     return a;
-- }


-- Try/catch/finally example
example2 = flatCode $ Java [ JavaBlock "try" [] ""
                           , JavaBlock "catch (ExcFoo nm) " [] ""
                           , JavaBlock "finally" [] ""
                           ]

-- *Cogen.Java> putStrLn example2
-- try{
-- }
-- catch (ExcFoo nm) {
-- }
-- finally{
-- }