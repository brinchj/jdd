module Cogen.Java.Jimple where


import Prelude ()
import CustomPrelude hiding (const)

import System.FilePath (takeDirectory, takeBaseName)

import qualified Data.Text as T
import qualified Data.Map as M

import Control.Monad.Writer (execWriter, tell)

import Parser (Desc(..), Class(..))
import Cogen.Java (Java(..), Javable(..), JavaStmt(..))

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


-- | Convert code to Java line with appended ";"
line :: Text -> Java
line st = Java [JavaStmt 0 $ st ++ ";"]


-- | Class path
path :: Text -> Text
path cp0 = T.map fix cp1
  where
    fix '/' = '.'
    fix c   = c

    lang = "java/lang/"
    cp1 = fromMaybe cp0 $ T.stripPrefix lang cp0

pathBS :: ByteString -> Text
pathBS = path . str

-- | Strings
str :: ByteString -> Text
str = fromUtf8


-- | Constants
const :: Constant -> Text
const (CDouble d)  = showT d
const (CFloat  f)  = showT f
const (CInt    i)  = showT i
const (CLong   l)  = showT l
const (CString s)  = showT s
const CNull        = "NULL"
const (CBoolean b) = if b then "true" else "false"


-- | Types
type_ :: Type -> Text
type_ t = case t of
  TArray n tp -> type_ tp ++ T.replicate n (T.pack "[]")
  TObject cp  -> path cp
  _ -> T.pack $ case t of
    TVoid -> "void"
    TByte -> "byte"
    TChar -> "char"
    TInt  -> "int"
    TBoolean -> "boolean"
    TFloat -> "float"
    TDouble -> "double"
    foo -> show foo


-- | Expressions
expr :: Expression Value -> Text
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
    let path1 = if it == IStatic then pathBS $ classPath cp else invoke it in
    if nm == "<init>" then
      path1 ++ intercalate "," (map value args)
    else
      concat [path1, ".", nm, "(", intercalate "," (map value args), ")"]

  _ -> error $ "Cogen.Java.Jimple.expr: " ++ show e

  where
    op f a b = concat [value a, " ", f, " ", value b]


-- | Flip a bool expression
flipExpr :: Expression Value -> Expression Value
flipExpr (EEq a b) = ENe a b -- =  to /=
flipExpr (EGe a b) = ELt a b -- >= to <
flipExpr (ELe a b) = EGt a b -- <= to >
flipExpr (ELt a b) = EGe a b -- <  to >=
flipExpr (ENe a b) = EEq a b -- /= to  =
flipExpr (EGt a b) = ELe a b -- >  to <=
flipExpr e = EEq (VExpr e) $ VConst $ CBoolean False


-- | Invocation types
invoke :: InvokeType Value -> Text
invoke (IVirtual   v) = value v
invoke (ISpecial   v) = value v
invoke (IInterface v) = value v


-- | Variables
var :: Variable Value -> Text
var (VarRef r)   = ref r
var (VarLocal l) = showT l


-- | References
ref :: Ref Value -> Text
ref r = case r of
  RInstanceField v (Desc nm _type) -> concat [value v, ".", str nm]
  RStaticField (Class cp) (Desc nm tp) -> concat [pathBS cp, ".", str nm]
  RArray v i -> concat [value v, "[", value i, "]"]
  RObject cl -> pathBS (classPath cl)
  RThis -> "this"
  _ -> error $ "Cogen.Java.Jimple, ref: " ++ show r


-- | Values
value :: Value -> Text
value (VConst c) = const c
value (VLocal v) = var v
value (VExpr  e) = expr e


-- | Inline code as a block
inline :: Javable s => s -> [JavaStmt]
inline s = code
  where
    Java code = toJava s

-- | Convert a stmt to Java code
stmtToJava s = case s of
  SNop -> Java []


  SAssign v val | var v == "_" -> line $ value val ++ " /* empty assign */ "
  SAssign v val -> line $ var v ++ " = " ++ value val

  SDeclare t v val -> line $ concat [type_ t, " ", var v, " = ", value val]

  SReturn mv -> line ("return " ++ maybe "" value mv)

  SDoWhile nm body v -> Java [
    JavaBlock (nm ++ ": do ") (inline body) (" while (" ++ value v ++ ");")]

  SBreak nm -> line $ "break " ++ nm
  SContinue nm -> line $ "continue " ++ nm

  SIfElse e0 left0 right0 -> Java $ execWriter $ do
    -- Flip condition when "main" body is empty (else is non-empty)
    let (e1, left1, right1) = if not $ null left0 then (e0, left0, right0)
                              else (flipExpr e0, right0, left0)
    tell [JavaBlock ("if (" ++ expr e1 ++ ") ") (inline left1) ""]
    unless (null right1) $
      tell [JavaBlock "else " (inline right1) ""]

  SSwitch nm v ls ->
    -- Regular cases
    let cases = [ JavaStmt (-4) ("case " ++ showT i ++ ":") : inline stmts
                | (Just i, stmts) <- ls ] in
    -- Default case
    let def   = [ JavaStmt (-4) "default:" : inline stmts
                | (Nothing, stmts) <- ls, not $ null stmts] in
    Java [
      JavaBlock (nm ++ ": switch(" ++ value v ++ ") ") (inline $ cases ++ def) ""
    ]

  STryCatch bd cs mfn ->
    -- Regular catch
    let catches = [ JavaBlock
                    ("catch (" ++ pathBS (classPath e) ++ " exc) ")
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
  foo -> Java [JavaStmt 0 $ "// unknown statement " ++ showT foo]


instance Javable LocalDecl where
  toJava (LocalDecl tp (Local nm)) =
    Java [ JavaStmt 0 $ type_ tp ++ " " ++ nm ++ ";" ]

instance Javable (JimpleMethod Value) where
  toJava = methodToJava

instance Javable CF.ClassFile where
  toJava = classToJava


methodToJava (Method sig locals0 idents stmts excs) =
  Java [JavaBlock methodHead code ""]
  where
    methodHead = modifiers ++
                 (if isInit then T.empty else type_ methodResult ++ " ") ++
                 name ++ "(" ++ params ++ ") "

    locals1  = filter (\(LocalDecl _ (Local nm)) -> nm `notElem` ("exc":argNames)) locals0
    argNames = take (length methodParams) $ map (("l"++).showT) ns
    argTypes = map type_ methodParams
    params   = T.intercalate ", " [
      ts ++ " " ++ nm | (ts, nm) <- zip argTypes argNames]

    header = toJava locals1
    body   = toJava stmts
    name   = if isInit then className else methodName

    className = T.pack $ takeBaseName $ T.unpack $ fromUtf8 $ classPath methodClass
    isInit = methodName == "<init>"

    Java code = header ++ body

    ns = if isStatic then [0..] else [1..]

    isStatic = FStatic `elem` methodAccess

    MethodSig{..} = sig

    modifiers = concat $ zipWith (++) (map showT methodAccess) $ repeat " "



phase1 = mapSuper . mapThis . mapInit . mapLabels
phase2 = mapFix $ mapCleanup . mapInline . mapAppendEmpty
phase3 = mapFix $ mapGotoIf . mapSwitch . mapWhile . mapElimGoto
phase4 = mapFix mapFixFinally . mapFix mapTryCatch

transform = foldl (flip (.)) id [phase1, phase2, phase3, phase4]

classToJava :: CF.ClassFile -> Java
classToJava cl = Java [ JavaStmt 0 $ "package " ++ clPackage ++ ";"
                      , emptyLine
                      , JavaBlock
                        ("class " ++ clName ++ " extends " ++ clSuper ++ " ")
                        (inline body) "" ]
  where
    clPackage = T.pack $ map slashToDot $ takeDirectory $ T.unpack clPath
    clName    = T.pack $ takeBaseName $ T.unpack clPath
    clPath    = fromUtf8 $ classPath $ CF.unClassRef classThis
    clSuper   = T.map slashToDot $ fromUtf8 $ classPath $ CF.unClassRef classSuper

    slashToDot '/' = '.'
    slashToDot chr = chr

    CF.ClassFile {..} = cl

    body = fields ++ methods

    fields = concat $ Java [emptyLine] :
             map field (M.elems $ CF.classFields cl)

    field (CF.AttrBlock blockFlags name desc _) =
      let typ = typeFromBS' desc
          flags = getFlags blockFlags
      in
      Java [JavaStmt 0 $
            concat (map ((++" ") . showT) flags) ++
            type_ typ ++ " " ++ str name ++ ";"]

    methods = concat $ Java [emptyLine] :
              map jimpleMethod (M.keys $ CF.classMethods cl)

    emptyLine = JavaStmt 0 ""

    jimpleMethod name =
       -- Transform method code and type local declarations
       toJava (simpleTyper $ transform meth) ++ Java [emptyLine]
      where
        (err, meth) = parseJimple cl name