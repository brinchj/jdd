{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , RecordWildCards
  #-}

module Cogen.Java.Jimple where

import Prelude hiding (const)
import Data.List

import System.FilePath (takeDirectory, takeBaseName)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Control.Monad.Writer (execWriter, tell, when, unless)

import Parser (Desc(..), Class(..))
import Cogen.Java (join, Java(..), Javable(..), JavaStmt(..))

import Jimple
import Jimple.Typing
import Jimple.Types
import Jimple.Maps

import qualified Parser as CF


-- s1 <- staticField
-- invoke I_special this MethodSig
-- return ()

--
instance Javable (LabelStmt Value) where
  toJava = stmtToJava


line st = Java [JavaStmt 0 $ st ++ ";"]


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

  E_add a b -> op "+" a b
  E_sub a b -> op "-" a b
  E_mul a b -> op "*" a b
  E_div a b -> op "*" a b

  E_length a -> value a ++ ".length"

  E_new rf -> "new " ++ ref rf
  E_newArray t i -> concat ["new ", type_ t, "[", value i, "]"]

  E_invoke it (MethodSig cp nm pars res _) args ->
    let path1 = if it == I_static then path $ classPath cp else invoke it in
    if nm == "<init>" then
      path1 ++ intercalate "," (map value args)
    else
      concat [path1, ".", str nm, "(", intercalate "," (map value args), ")"]

  _ -> error $ "expr: " ++ show e

  where
    op f a b = concat [value a, " ", f, " ", value b]


-- flip a bool expression
flipExpr :: Expression Value -> Expression Value
flipExpr (E_eq a b) = E_ne a b -- =  to /=
flipExpr (E_ge a b) = E_lt a b -- >= to <
flipExpr (E_le a b) = E_gt a b -- <= to >
flipExpr (E_lt a b) = E_ge a b -- <  to >=
flipExpr (E_ne a b) = E_eq a b -- /= to  =
flipExpr (E_gt a b) = E_le a b -- >  to <=
flipExpr e = E_eq (VExpr e) $ VConst $ C_boolean False


-- invoke
invoke (I_virtual   v) = value v
invoke (I_special   v) = value v
invoke (I_interface v) = value v


-- variable
var (VarRef r)   = ref r
var (VarLocal l) = show l


-- reference
ref r = case r of
  R_instanceField v (Desc nm _type) -> concat [value v, ".", str nm]
  R_staticField (Class cp) (Desc nm tp) -> concat [path cp, ".", str nm]
  R_array v i -> concat [value v, "[", value i, "]"]
  R_object cl -> path (classPath cl) ++ "()"  -- TODO: R_object without () ??
  R_this -> "this"
  _ -> error $ "Cogen.Java.Jimple, ref: " ++ show r

-- value
value (VConst c) = const c
value (VLocal v) = var v
value (VExpr  e) = expr e

inline s = code
  where
    Java code = toJava s

stmtToJava (lbl, s) = case s of
  S_nop -> Java []


  S_assign v val | var v == "_" -> line $ value val ++ " /* empty assign */ "
  S_assign v val -> line $ var v ++ " = " ++ value val

  S_return v   -> line $ concat ["return (", value v, ")"]
  S_returnVoid -> line "return"

  S_doWhile nm body v -> Java [
    JavaBlock (nm ++ ": do ") (inline body) (concat [" while (", value v, ");"])]

  S_break nm -> line $ "break " ++ nm
  S_continue nm -> line $ "continue " ++ nm

  S_ifElse e0 left0 right0 -> Java $ execWriter $ do
    -- Flip condition when "main" body is empty (else is non-empty)
    let (e1, left1, right1) = if not $ null left0 then (e0, left0, right0)
                              else (flipExpr e0, right0, left0)
    tell [JavaBlock (concat ["if (", expr e1, ") "]) (inline left1) ""]
    unless (null right1) $
      tell [JavaBlock "else " (inline right1) ""]

  S_switch nm v ls ->
    -- Regular cases
    let cases = [ JavaStmt (-4) (concat ["case ", show i, ":"]) : inline stmts
                | (Just i, stmts) <- ls ] in
    -- Default case
    let def   = [ JavaStmt (-4) "default:" : inline stmts
                | (Nothing, stmts) <- ls, not $ null stmts] in
    Java [
      JavaBlock (concat [nm, ": switch(", value v, ") "]) (inline $ cases ++ def) ""
    ]

  foo -> error $ "stmtToJava: Unknown statement " ++ show foo


instance Javable LocalDecl where
  toJava (LocalDecl tp (Local nm)) = Java [
    JavaStmt 0 $ concat [ type_ tp, " ", nm, ";"] ]

instance Javable (JimpleMethod Value) where
  toJava = methodToJava

instance Javable CF.ClassFile where
  toJava = classToJava


methodToJava (Method sig locals0 idents stmts excs) =
  Java [JavaBlock methodHead code ""]
  where
    methodHead = modifiers ++ concat [
      type_ methodResult, " ", name, "(", params, ") "]

    locals1 = defThis ++
              filter (\(LocalDecl _ (Local nm)) -> nm `notElem` argNames) locals0
    argNames = take (length methodParams) $ map (('l':).show) ns
    argTypes = map type_ methodParams
    params   = intercalate ", " [
      concat [ts, " ", nm] | (ts, nm) <- zip argTypes argNames]

    header = toJava locals1
    body   = if isStatic then toJava stmts else toJava $ setThis : stmts
    name   = if str methodName == "<init>" then className else str methodName

    defThis = if isStatic then []
              else [LocalDecl (T_object $ classPath methodClass) $ Local "l0"]
    setThis = (Nothing, S_assign (VarLocal $ Local "l0")
                        (VLocal $ VarRef R_this))

    className = takeBaseName $ B.unpack $ classPath methodClass

    Java code = join [header, body]

    ns = if isStatic then [0..] else [1..]

    isStatic = F_static `elem` methodAccess

    MethodSig{..} = sig

    modifiers = concat $ zipWith (++) (map show methodAccess) $ repeat " "



phase1 = mapCorrectLabels
phase2 = mapFix $ mapCleanup . mapInline . mapAppendEmpty
phase3 = mapFix $ mapSwitch . mapWhile . mapGotoIf . mapElimGoto

classToJava :: CF.ClassFile -> Java
classToJava cl = Java [ JavaStmt 0 $ "package " ++ clPackage ++ ";"
                      , emptyLine
                      , JavaBlock
                        (concat ["class ", clName, " extends ", clSuper, " "])
                        (inline body) "" ]
  where
    clPackage = map slashToDot $ takeDirectory clPath
    clName    = takeBaseName clPath
    clPath    = B.unpack $ classPath $ CF.unClassRef classThis
    clSuper   = map slashToDot $ B.unpack $ classPath $ CF.unClassRef classSuper

    slashToDot '/' = '.'
    slashToDot chr = chr

    CF.ClassFile {..} = cl

    body = join [fields, methods]

    fields = join $ Java [emptyLine] :
             (map field $ M.elems $ CF.classFields cl)

    field (CF.AttrBlock blockFlags name desc _) =
      let typ = typeFromBS' desc
          flags = getFlags blockFlags
      in
      Java [JavaStmt 0 $ concat ([show flag ++ " " | flag <- flags] ++
                                 [type_ typ, " ", str name, ";"])]

    methods = join $ Java [emptyLine] :
              (map jimpleMethod $ M.keys $ CF.classMethods cl)

    emptyLine = JavaStmt 0 ""

    jimpleMethod name =
      let (err, meth0) = parseJimple cl name
          -- Code rewriting
          transform = phase3 . phase2 . phase1
          meth1 = transform meth0
      in
       -- Type local declarations
       join $ [toJava $ simpleTyper meth1, Java [emptyLine]]