module Cogen.Java where

import Cogen
import Data.List


-- We can construct Java-code from a list of Java statements
data Java = Java [JavaStmt]
          deriving Show

instance Codeable Java where
  toCode ind (Java stmts) = concat $ map (toCode ind) stmts


-- Data types to represent blocks of Java-statements
data JavaStmt = JavaBlock String [JavaStmt] String
              | JavaStmt  { javaIndent :: Int
                          , javaStmt   :: String }
              deriving Show

-- Blocks of Java-statements can be used to construct Java-code
instance Codeable JavaStmt where
  toCode ind (JavaStmt relInd stmt) =
    [[replicate (ind+relInd) ' ', stmt]]

  toCode ind (JavaBlock left stmts right) =
    let indS = replicate ind ' ' in
    [ [indS, left, "{"] ] ++
    (concat $ map (toCode $ 4 + ind) stmts) ++
    [ [indS, "}", right ] ]



-- A typeclass for things we can turn into Java
class Javable a where
  toJava :: a -> Java

instance Javable a => Javable [a] where
  toJava l = join $ map toJava l

instance Javable Java where
  toJava = id

instance Javable JavaStmt where
  toJava = Java . return

append :: Java -> Java -> Java
append (Java a) (Java b) = Java $ a ++ b

join :: [Java] -> Java
join = foldl' append (Java [])


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