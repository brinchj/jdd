module Jimple.Types where

import qualified Data.ByteString as B
import qualified Parser as CF

data JimpleMethod = Method
                    { methodLocalDecls :: [LocalDecl]
                    , methodIdentStmts :: [IdentStmt]
                    , methodStmts      :: [(Maybe Label, Stmt)]
                    , methodExcepts    :: [Except] }
                  deriving Show

data IdentStmt = IStmt Local Ref
               deriving Show

data LocalDecl = LocalDecl Type String
               deriving Show

data Except = Except Ref Label Label Label
            deriving Show


data Stmt = S_breakpoint
          | S_assign Variable Value
          | S_enterMonitor Value
          | S_exitMonitor  Value
          | S_goto Label
          | S_if Expression Label   -- Only condition expressions are allowed
          | S_invoke InvokeType MethodSignature [Value] Variable
          | S_lookupSwitch Label [(Int, Label)]
          | S_nop
          | S_ret Local
          | S_return Value
          | S_returnVoid
          | S_tableSwitch Value Label [(Int, Label)]
          | S_throw Value

data Value = VConst Constant
           | VLocal Variable
           | VExpr  Expression

data Label = Label Integer
instance Show Label where show (Label l) = show l


data Local = Local String
instance Show Local where show (Local s) = s


data Constant = C_double Double
              | C_float  Double
              | C_int    Integer
              | C_long   Integer
              | C_string B.ByteString
              | C_null
              deriving Show

data Variable = VarRef Ref | VarLocal Local

data Ref = R_caughtException
         | R_parameter     Integer
         | R_this
         | R_array         Value Value
         | R_instanceField Value CF.Desc
         | R_staticField      CF.Desc
         | R_object        CF.Class
         deriving Show


data Expression = E_eq Value Value -- Conditions
                | E_ge Value Value
                | E_le Value Value
                | E_lt Value Value
                | E_ne Value Value
                | E_gt Value Value

                | E_add  Value Value -- Binary ops
                | E_sub  Value Value
                | E_and  Value Value
                | E_or   Value Value
                | E_xor  Value Value
                | E_shl  Value Value
                | E_shr  Value Value
                | E_ushl Value Value
                | E_ushr Value Value
                | E_cmp  Value Value
                | E_cmpg Value Value
                | E_cmpl Value Value
                | E_mul Value Value
                | E_div Value Value
                | E_rem Value Value

                | E_length Value
                | E_cast   Type Value
                | E_instanceOf Value Ref
                | E_newArray Type Value
                | E_new Ref
                | E_newMultiArray Type Value [Value] -- TODO: empty dims?


data InvokeType = I_interface Value
                | I_special   Value
                | I_virtual   Value
                | I_static
                deriving Show

data MethodSignature = MethodSig
                       { methodClass  :: CF.Class
                       , methodName   :: B.ByteString
                       , methodParams :: [Type]
                       , methodResult :: Type         }
                     deriving Show

data Type = T_byte | T_char  | T_int | T_boolean | T_short
          | T_long | T_float | T_double
          | T_object String | T_addr | T_void
          | T_array Type
          deriving (Show, Eq)




instance Show Stmt where
  show (S_breakpoint)    = "breakpoint"

  show (S_assign x a)    = show x ++ " <- " ++ show a

  show (S_enterMonitor i) = "enterMonitor " ++ show i
  show (S_exitMonitor  i) = "exitMonitor " ++ show i

  show (S_goto lbl)      = "goto " ++ show lbl
  show (S_if con lbl)    = "if (" ++ show con ++ ") " ++ show lbl

  show (S_invoke t m ims v) = show v ++ " <- invoke " ++ show t ++ " " ++
                              show m ++ " " ++ show ims

  show (S_lookupSwitch lbl ls) = "lswitch " ++ show lbl ++ " " ++ show ls

  show (S_nop)           = "nop"

  show (S_ret v)         = "return (" ++ show v ++ ")"
  show (S_return i)      = "return (" ++ show i ++ ")"
  show (S_returnVoid)    = "return"

  show (S_tableSwitch i lbl ls) = "tswitch" ++ show i ++ " " ++ show lbl ++ " "
                                  ++ show ls

  show (S_throw i) = "throw " ++ show i


instance Show Variable where
  show (VarRef   ref) = '@' : show ref
  show (VarLocal v  ) = show v

instance Show Value where
  show (VConst c) = show c
  show (VLocal l) = show l
  show (VExpr  e) = show e


instance Show Expression where
  show (E_eq a b) = show a ++ " == " ++ show b
  show (E_ge a b) = show a ++ " >= " ++ show b
  show (E_le a b) = show a ++ " <= " ++ show b
  show (E_ne a b) = show a ++ " /= " ++ show b
  show (E_lt a b) = show a ++ " < " ++ show b
  show (E_gt a b) = show a ++ " > " ++ show b

  show (E_add a b) = show a ++ " + " ++ show b
  show (E_sub a b) = show a ++ " - " ++ show b
  show (E_and a b) = show a ++ " & " ++ show b
  show (E_or  a b) = show a ++ " | " ++ show b
  show (E_xor a b) = show a ++ " ^ " ++ show b
  show (E_shl a b) = show a ++ " shl " ++ show b
  show (E_shr a b) = show a ++ " shr " ++ show b
  show (E_ushl a b) = show a ++ " ushl " ++ show b
  show (E_ushr a b) = show a ++ " ushr " ++ show b
  show (E_cmp a b) = show a ++ " cmp " ++ show b
  show (E_cmpg a b) = show a ++ " cmpg " ++ show b
  show (E_cmpl a b) = show a ++ " cmpl " ++ show b

  show (E_mul a b) = show a ++ " * " ++ show b
  show (E_div a b) = show a ++ " / " ++ show b
  show (E_rem a b) = show a ++ " rem " ++ show b

  show (E_length a) = "len " ++ show a
  show (E_cast t a) = "(" ++ show t ++ ") " ++ show a
  show (E_instanceOf i r) = show i ++ " instanceOf " ++ show r
  show (E_newArray t i) = "new " ++ show t ++ "[" ++ show i ++ "]"
  show (E_new r) = "new " ++ show r
  show (E_newMultiArray t i is) = "new " ++ show t ++ "(" ++ show (i, is) ++ ")"


