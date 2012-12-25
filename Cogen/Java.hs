module Cogen.Java where


import Data.List


-- A value is Javable when it can be transformed into a list of line-pieces
class Javable a where
  toJava :: Int -> a -> [[String]]


-- We can construct Java-code from a list of Java statements
data Java = Java [JavaStmt]
instance Javable Java where
  toJava ind (Java stmts) = concat $ map (toJava ind) stmts



-- Data types to represent blocks of Java-statements
data JavaStmt = JavaBlock String [JavaStmt] String
              | JavaStmt  { javaIndent :: Int
                          , javaStmt   :: String }

-- Blocks of Java-statements can be used to construct Java-code
instance Javable JavaStmt where
  toJava ind (JavaStmt relInd stmt) =
    [[replicate (ind+relInd) ' ', stmt]]

  toJava ind (JavaBlock left stmts right) =
    let indS = replicate ind ' ' in
    [ [indS, left, "{"] ] ++
    (concat $ map (toJava $ 4 + ind) stmts) ++
    [ [indS, "}", right ] ]


-- Convert our internal representation of Java-code into real, "flat" Java-code
flatJava :: Javable a => a -> String
flatJava stmt = intercalate "\n" $ map concat java
  where
    java = toJava 0 stmt


-- A simple example
example1 = flatJava $
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
example2 = flatJava $ Java [ JavaBlock "try" [] ""
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
