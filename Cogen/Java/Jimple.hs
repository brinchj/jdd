{-# LANGUAGE FlexibleInstances
           , RecordWildCards
  #-}

module Cogen.Java.Jimple where

import Prelude hiding (const)
import Data.List

import qualified Data.ByteString.Char8 as B

import Parser (Desc(..), Class(..))
import Cogen.Java (join, Java(..), Javable(..), JavaStmt(..))
import Jimple.Types



-- s1 <- staticField
-- invoke I_special this MethodSig
-- return ()

--
instance Javable (LabelStmt Value) where
  toJava = stmtToJava


line st = Java $ [JavaStmt 0 $ st ++ ";"]


-- class path
path cp0 = str $ B.map fix cp1
  where
    fix '/' = '.'
    fix c   = c

    cp1 = if B.take 10 cp0 == "java/lang/" then B.drop 10 cp0 else cp0


-- string (TODO: UTF8?)
str = B.unpack


-- constant
const (C_double d)  = show d
const (C_float  f)  = show f
const (C_int    i)  = show i
const (C_long   l)  = show l
const (C_string s)  = show s
const C_null        = "NULL"
const (C_boolean b) = if b then "true" else "false"

-- types
type_ t = case t of
  T_array n tp -> concat $ type_ tp:replicate n "[]"
  T_object cp  -> path cp
  T_void -> "void"
  T_int  -> "int"
  foo -> show foo


-- expression
expr e = case e of
  E_eq a b -> op "==" a b
  E_ge a b -> op ">=" a b
  E_le a b -> op "<=" a b
  E_lt a b -> op "<"  a b
  E_ne a b -> op "!=" a b
  E_gt a b -> op ">"  a b

  E_invoke it (MethodSig _cl nm pars res) args ->
    concat [invoke it, ".", str nm, "(", intercalate "," (map value args), ")"]

  where
    op f a b = concat [value a, " ", f, " ", value b]

-- invoke
invoke (I_virtual v) = value v
invoke (I_special v) = value v


-- variable
var (VarRef r)   = ref r
var (VarLocal l) = show l


-- reference
ref r = case r of
  R_staticField (Class cp) (Desc nm tp) -> concat [path cp, ".", str nm]


-- value
value (VConst c) = const c
value (VLocal v) = var v
value (VExpr  e) = expr e

inline s = code
  where
    Java code = toJava s

stmtToJava (lbl, s) = case s of
  S_nop -> Java []


  S_assign v val | var v == "_" -> line $ value val
  S_assign v val -> line $ var v ++ " = " ++ value val

  S_return v   -> line $ concat ["return (", value v, ")"]
  S_returnVoid -> line "return"

  S_doWhile nm body v -> Java $ [
    JavaBlock (nm ++ ": do ") (inline body) (concat [" while (", value v, ")"])]

  S_ifElse e left right -> Java $ [
    JavaBlock (concat ["if (", expr e, ") "]) (inline left) "",
    JavaBlock "else " (inline right) "" ]

  foo -> error $ "stmtToJava: Unkown statement " ++ show foo


instance Javable LocalDecl where
  toJava (LocalDecl tp (Local nm)) = Java [
    JavaStmt 0 $ concat [ type_ tp, " ", nm, ";"] ]

instance Javable (JimpleMethod Value) where
  toJava = methodToJava


methodToJava (Method sig locals0 idents stmts excs) =
  Java $ [JavaBlock methodHead code ""]
  where
    methodHead = concat [
      type_ methodResult, " ", str methodName, "(", params, ") "]

    locals1 = filter (\(LocalDecl _ (Local nm)) -> nm `notElem` argNames) locals0
    argNames = take (length methodParams) $ map (('l':).show) [0..]
    argTypes = map type_ methodParams
    params   = intercalate ", " [
      concat [ts, " ", nm] | (ts, nm) <- zip argTypes argNames]

    header = toJava locals1
    body   = toJava stmts

    Java code = join [header, body]

    MethodSig{..} = sig