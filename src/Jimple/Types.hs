{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
  #-}

module Jimple.Types where

import Prelude ()
import CustomPrelude

import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Parser as CF


type LabelStmt v = (Maybe Label, Stmt v)


data JimpleMethod v = Method
                      { methodSig        :: MethodSignature
                      , methodLocalDecls :: [LocalDecl]
                      , methodIdentStmts :: [IdentStmt v]
                      , methodStmts      :: [LabelStmt v]
                      , methodExcepts    :: [Except v] }
                  deriving (Eq, Show)

data IdentStmt v = IStmt Local (Ref v)
               deriving (Eq, Ord, Show, Functor, F.Foldable)

data LocalDecl = LocalDecl Type Local
               deriving (Eq, Ord, Show)

data Except v = Except (Ref v) Label Label Label
            deriving (Eq, Ord, Show, Functor, F.Foldable)

data ExceptTable = ExceptTable [ExceptEntry]
                 deriving (Eq, Ord, Show)
data ExceptEntry =
  ExceptEntry { exceptFrom   :: Integer
              , exceptTo     :: Integer
              , exceptTarget :: Integer
              , exceptID     :: Integer
              }
  deriving (Eq, Ord, Show)


data Stmt v = SBreakpoint
            | SAssign       (Variable v) v
            | SDeclare Type (Variable v) v
            | SEnterMonitor v
            | SExitMonitor  v
            | SGoto Label
            | SIf (Expression v) Label   -- Only condition expressions are allowed
            | SLookupSwitch v Label [(Integer, Label)]
            | SNop
            | SReturn (Maybe v)
            | STableSwitch v Label [(Integer, Label)]
            | SThrow v
            -- Below are statements for transitioning from Jimple to Java
            | SIfElse (Expression v) [LabelStmt v] [LabelStmt v]
            -- We used labeled continue/break to tie the action to the loop
            | SDoWhile  Text [LabelStmt v] v
            | SBreak    Text
            | SContinue Text
            | SSwitch   Text v [(Maybe Integer, [LabelStmt v])]
            -- Low-level exception hints, catch (finally is 'SCatch Nothing')
            | STry      (Integer, Integer) Integer
            | SCatch    (Integer, Integer) ExceptEntry (Maybe CF.Class)
            | STryCatch [LabelStmt v]
              [(CF.Class, [LabelStmt v])] -- catch exception
              (Maybe [LabelStmt v])       -- finally
            -- Mainly for debugging
            | SComment Text
            deriving (Eq, Ord, Functor, F.Foldable)


data Value = VConst Constant
           | VLocal (Variable Value)
           | VExpr  (Expression Value)
           deriving (Eq, Ord)

data Label = Label Integer
           deriving (Eq, Ord)

instance Show     Label where show (Label l) = show l
instance Num      Label where
  fromInteger = Label
  (+)         = labelOp (+)
  (*)         = labelOp (*)
  signum (Label n) = Label $ signum n
  abs    (Label n) = Label $ abs n

instance Real Label where
  toRational (Label a) = fromIntegral a

instance Enum Label where
  toEnum = Label . fromIntegral
  fromEnum (Label a) = fromIntegral a

instance Integral Label where
  toInteger (Label a) = a
  quot                = labelOp quot
  rem                 = labelOp rem
  div                 = labelOp div
  mod                 = labelOp mod
  quotRem (Label a) (Label b) = let (c, d) = a `quotRem` b in (Label c, Label d)
  divMod  (Label a) (Label b) = let (c, d) = a `divMod`  b in (Label c, Label d)


labelOp f (Label a) (Label b) = Label $ a `f` b


data Local = Local Text
           deriving (Eq, Ord)
instance Show Local where show (Local s) = T.unpack s


data AccessFlag = FPublic
                | FPrivate
                | FProtected
                | FStatic
                | FFinal
                | FSynchronized
                | FBridge
                | FVarargs
                | FNative
                | FAbstract
                | FStrict
                | FSynthetic
                deriving (Eq, Ord)

data Constant = CDouble Double
              | CFloat  Double
              | CInt    Integer
              | CLong   Integer
              | CString Text
              | CNull
              | CBoolean Bool
              deriving (Eq, Ord, Show)

data Variable v = VarRef (Ref v)
                | VarLocal Local
                deriving (Eq, Ord, Functor, F.Foldable)

data Ref v = RCaughtException
           | RParameter     Integer
           | RThis
           | RSuper
           | RArray         v v
           | RInstanceField v CF.Desc
           | RStaticField     CF.Class CF.Desc
           | RObject          CF.Class
           deriving (Eq, Ord, Show, Functor, F.Foldable)


data Expression v = EEq v v -- Conditions
                  | EGe v v
                  | ELe v v
                  | ELt v v
                  | ENe v v
                  | EGt v v

                  | EAdd  v v -- Binary ops
                  | ESub  v v
                  | EAnd  v v
                  | EOr   v v
                  | EXor  v v
                  | EShl  v v
                  | EShr  v v
                  | EUshl v v
                  | EUshr v v
                  | ECmp  v v
                  | ECmpg v v
                  | ECmpl v v
                  | EMul  v v
                  | EDiv  v v
                  | ERem  v v

                  | ELength        v
                  | EInstanceOf    v    (Ref v)
                  | ECast          Type v
                  | ENewArray      Type v
                  | ENewMultiArray Type v [v] -- TODO: empty dims?
                  | ENew           (Ref v) [v]
                  | EInvoke (InvokeType v) MethodSignature [v]
                  deriving (Eq, Ord, Functor, F.Foldable)

data InvokeType v = IInterface v
                  | ISpecial   v
                  | IVirtual   v
                  | IStatic
                deriving (Eq, Ord, Show, Functor, F.Foldable)

data MethodSignature = MethodSig
                       { methodClass  :: CF.Class
                       , methodName   :: Text
                       , methodAccess :: [ AccessFlag ]
                       , methodParams :: [Type]
                       , methodResult :: Type
                       }
                     deriving (Eq, Ord, Show)

data Type = TByte | TChar  | TInt | TBoolean | TShort
          | TLong | TFloat | TDouble
          | TObject Text | TAddr | TVoid
          | TArray Int Type
          | TUnknown
          deriving (Eq, Ord, Show)




instance Show v => Show (Stmt v) where
  show (SBreakpoint)    = "breakpoint"

  show (SAssign    x a) = show x ++ " <- " ++ show a
  show (SDeclare t x a) = concat [show t, " ", show x, " <- ", show a]

  show (SEnterMonitor i) = "enterMonitor " ++ show i
  show (SExitMonitor  i) = "exitMonitor " ++ show i

  show (SGoto lbl)      = "goto " ++ show lbl
  show (SIf con lbl)    = "if (" ++ show con ++ ") " ++ show lbl
  show (SIfElse c a b)  = concat ["if (", show c, ") "
                                  , show a, " else "
                                  , show b]

  show (SContinue name) = "continue " ++ T.unpack name
  show (SBreak    name) = "break "    ++ T.unpack name

  show (SDoWhile name body cond) = concat [T.unpack name, ": do ", show body
                                           , " while (", show cond, ")"]

  show (SLookupSwitch v lbl ls) = "lswitch " ++ show v ++ " " ++ show lbl ++ " " ++ show ls

  show (SNop)           = "nop"

  show (SReturn mv) = "return (" ++ maybe "" show mv ++ ")"

  show (STableSwitch i lbl ls) = "tswitch" ++ show i ++ " " ++ show lbl ++ " "
                                  ++ show ls

  show (SSwitch name v cs) = concat [T.unpack name, ": switch (", show v, ") ", show cs]

  show (SThrow i) = "throw " ++ show i

  show (STry   s t) = "try " ++ show s ++ " -> " ++ show t
  show (SCatch s _ e) = concat ["catch ", show s, " (", show e, ")"]

  show (STryCatch body catches finally) =
    concat ["tryCatch ", show body, show catches, " finally ", show finally]

  show (SComment s) = "comment " ++ T.unpack s

instance Show v => Show (Variable v) where
  show (VarRef   ref) = '@' : show ref
  show (VarLocal var) = show var

instance Show Value where
  show (VConst c) = show c
  show (VLocal l) = show l
  show (VExpr  e) = show e


instance Show AccessFlag where
  show FPublic = "public"
  show FPrivate = "private"
  show FProtected = "protected"
  show FStatic = "static"
  show FFinal = "final"
  show FSynchronized = "synchronized"
  show FBridge = "INTERNAL_bridge"
  show FVarargs = "INTERNAL_varargs"
  show FNative = "native"
  show FAbstract = "abstract"
  show FStrict = "strictfp"
  show FSynthetic = "INTERNAL_synthetic"


instance Show v => Show (Expression v) where
  show (EEq a b) = show a ++ " == " ++ show b
  show (EGe a b) = show a ++ " >= " ++ show b
  show (ELe a b) = show a ++ " <= " ++ show b
  show (ENe a b) = show a ++ " /= " ++ show b
  show (ELt a b) = show a ++ " < " ++ show b
  show (EGt a b) = show a ++ " > " ++ show b

  show (EAdd a b) = show a ++ " + " ++ show b
  show (ESub a b) = show a ++ " - " ++ show b
  show (EAnd a b) = show a ++ " & " ++ show b
  show (EOr  a b) = show a ++ " | " ++ show b
  show (EXor a b) = show a ++ " ^ " ++ show b
  show (EShl a b) = show a ++ " shl " ++ show b
  show (EShr a b) = show a ++ " shr " ++ show b
  show (EUshl a b) = show a ++ " ushl " ++ show b
  show (EUshr a b) = show a ++ " ushr " ++ show b
  show (ECmp a b) = show a ++ " cmp " ++ show b
  show (ECmpg a b) = show a ++ " cmpg " ++ show b
  show (ECmpl a b) = show a ++ " cmpl " ++ show b

  show (EMul a b) = show a ++ " * " ++ show b
  show (EDiv a b) = show a ++ " / " ++ show b
  show (ERem a b) = show a ++ " rem " ++ show b

  show (ELength a) = "length " ++ show a
  show (ECast t a) = "(" ++ show t ++ ") " ++ show a
  show (EInstanceOf i r) = show i ++ " instanceOf " ++ show r
  show (ENewArray t i) = "newArray " ++ show t ++ "[" ++ show i ++ "]"
  show (ENew r as) = "new " ++ show r ++ show as
  show (ENewMultiArray t i is) = "newMArray " ++ show t ++ "(" ++ show (i, is) ++ ")"
  show (EInvoke t m ims) = concat ["invoke ", show t, " "
                                   , show m, " ", show ims]





