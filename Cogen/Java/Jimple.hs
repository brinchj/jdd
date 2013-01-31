{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , RecordWildCards
  #-}

module Cogen.Java.Jimple where

import Prelude hiding (const)
import Data.List
import Data.Maybe

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
-- invoke ISpecial this MethodSig
-- return ()

--
instance Javable (LabelStmt Value) where
  toJava = stmtToJava . snd


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
const (CDouble d)  = show d
const (CFloat  f)  = show f
const (CInt    i)  = show i
const (CLong   l)  = show l
const (CString s)  = show s
const CNull        = "NULL"
const (CBoolean b) = if b then "true" else "false"

-- types
type_ t = case t of
  TArray n tp -> concat $ type_ tp:replicate n "[]"
  TObject cp  -> path cp
  TVoid -> "void"
  TByte -> "byte"
  TChar -> "char"
  TInt  -> "int"
  TBoolean -> "boolean"
  TFloat -> "float"
  TDouble -> "double"
  foo -> show foo


-- expression
expr e = case e of
  EEq a b -> op "==" a b
  EGe a b -> op ">=" a b
  ELe a b -> op "<=" a b
  ELt a b -> op "<"  a b
  ENe a b -> op "!=" a b
  EGt a b -> op ">"  a b

  EAdd a b -> op "+" a b
  ESub a b -> op "-" a b
  EMul a b -> op "*" a b
  EDiv a b -> op "/" a b

  ERem a b -> op "%" a b

  ELength a -> value a ++ ".length"

  ENew rf args -> concat $ ["new ", ref rf, "("] ++ map value args ++ [")"]
  ENewArray t i -> concat ["new ", type_ t, "[", value i, "]"]

  EInvoke it (MethodSig cp nm pars res _) args ->
    let path1 = if it == IStatic then path $ classPath cp else invoke it in
    if nm == "<init>" then
      path1 ++ intercalate "," (map value args)
    else
      concat [path1, ".", str nm, "(", intercalate "," (map value args), ")"]

  _ -> error $ "Cogen.Java.Jimple.expr: " ++ show e

  where
    op f a b = concat [value a, " ", f, " ", value b]


-- flip a bool expression
flipExpr :: Expression Value -> Expression Value
flipExpr (EEq a b) = ENe a b -- =  to /=
flipExpr (EGe a b) = ELt a b -- >= to <
flipExpr (ELe a b) = EGt a b -- <= to >
flipExpr (ELt a b) = EGe a b -- <  to >=
flipExpr (ENe a b) = EEq a b -- /= to  =
flipExpr (EGt a b) = ELe a b -- >  to <=
flipExpr e = EEq (VExpr e) $ VConst $ CBoolean False


-- invoke
invoke (IVirtual   v) = value v
invoke (ISpecial   v) = value v
invoke (IInterface v) = value v


-- variable
var (VarRef r)   = ref r
var (VarLocal l) = show l


-- reference
ref r = case r of
  RInstanceField v (Desc nm _type) -> concat [value v, ".", str nm]
  RStaticField (Class cp) (Desc nm tp) -> concat [path cp, ".", str nm]
  RArray v i -> concat [value v, "[", value i, "]"]
  RObject cl -> path (classPath cl)
  RThis -> "this"
  _ -> error $ "Cogen.Java.Jimple, ref: " ++ show r

-- value
value (VConst c) = const c
value (VLocal v) = var v
value (VExpr  e) = expr e

inline s = code
  where
    Java code = toJava s

stmtToJava s = case s of
  SNop -> Java []


  SAssign v val | var v == "_" -> line $ value val ++ " /* empty assign */ "
  SAssign v val -> line $ var v ++ " = " ++ value val

  SDeclare t v val -> line $ concat [type_ t, " ", var v, " = ", value val]

  SReturn mv -> line ("return " ++ maybe "" value mv)

  SDoWhile nm body v -> Java [
    JavaBlock (nm ++ ": do ") (inline body) (concat [" while (", value v, ");"])]

  SBreak nm -> line $ "break " ++ nm
  SContinue nm -> line $ "continue " ++ nm

  SIfElse e0 left0 right0 -> Java $ execWriter $ do
    -- Flip condition when "main" body is empty (else is non-empty)
    let (e1, left1, right1) = if not $ null left0 then (e0, left0, right0)
                              else (flipExpr e0, right0, left0)
    tell [JavaBlock (concat ["if (", expr e1, ") "]) (inline left1) ""]
    unless (null right1) $
      tell [JavaBlock "else " (inline right1) ""]

  SSwitch nm v ls ->
    -- Regular cases
    let cases = [ JavaStmt (-4) (concat ["case ", show i, ":"]) : inline stmts
                | (Just i, stmts) <- ls ] in
    -- Default case
    let def   = [ JavaStmt (-4) "default:" : inline stmts
                | (Nothing, stmts) <- ls, not $ null stmts] in
    Java [
      JavaBlock (concat [nm, ": switch(", value v, ") "]) (inline $ cases ++ def) ""
    ]

  STryCatch bd cs mfn ->
    -- Regular catch
    let catches = [ JavaBlock
                    (concat ["catch (", path $ classPath e, " exc) "])
                    (inline stmts) ""
                  | (e, stmts) <- cs ] in
    -- Finally clause
    let finally = [ JavaBlock "finally " (inline stmts) ""
                  | stmts <- maybeToList mfn, not $ null stmts ] in
    Java $ [
      JavaBlock "try " (inline bd) ""
      ] ++ catches ++ finally

  SThrow exc -> line $ "throw " ++ value exc

  SComment s -> Java [JavaStmt 0 $ "/* " ++ s ++ " */"]

  -- foo -> error $ "stmtToJava: Unknown statement " ++ show foo
  foo -> Java [JavaStmt 0 $ "// unknown statement " ++ show foo]


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
    methodHead = modifiers ++
                 (if isInit then [] else type_ methodResult ++ " ") ++
                 concat [name, "(", params, ") "]

    locals1  = filter (\(LocalDecl _ (Local nm)) -> nm `notElem` ("exc":argNames)) locals0
    argNames = take (length methodParams) $ map (('l':).show) ns
    argTypes = map type_ methodParams
    params   = intercalate ", " [
      concat [ts, " ", nm] | (ts, nm) <- zip argTypes argNames]

    header = toJava locals1
    body   = toJava stmts
    name   = if isInit then className else str methodName

    className = takeBaseName $ B.unpack $ classPath methodClass
    isInit = str methodName == "<init>"

    Java code = join [header, body]

    ns = if isStatic then [0..] else [1..]

    isStatic = FStatic `elem` methodAccess

    MethodSig{..} = sig

    modifiers = concat $ zipWith (++) (map show methodAccess) $ repeat " "



phase1 = mapSuper . mapThis . mapInit . mapLabels
phase2 = mapFix $ mapCleanup . mapInline . mapAppendEmpty
phase3 = mapFix $ mapGotoIf . mapSwitch . mapWhile . mapElimGoto
phase4 = mapFix mapFixFinally . mapFix mapTryCatch

transform = foldl (flip (.)) id [phase1, phase2, phase3, phase4]

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
             map field (M.elems $ CF.classFields cl)

    field (CF.AttrBlock blockFlags name desc _) =
      let typ = typeFromBS' desc
          flags = getFlags blockFlags
      in
      Java [JavaStmt 0 $ concat ([show flag ++ " " | flag <- flags] ++
                                 [type_ typ, " ", str name, ";"])]

    methods = join $ Java [emptyLine] :
              map jimpleMethod (M.keys $ CF.classMethods cl)

    emptyLine = JavaStmt 0 ""

    jimpleMethod name =
      let (err, meth0) = parseJimple cl name
          -- Code rewriting
          meth1 = transform meth0
      in
       -- Type local declarations
       join [toJava $ simpleTyper meth1, Java [emptyLine]]