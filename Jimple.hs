module Jimple where

import qualified Data.ByteString.Char8 as B


data JimpleMethod = Method
                    [LocalDecl]
                    [IdentStmt]
                    [(Maybe Label, Stmt)]
                    [Except]

data IdentStmt = IStmt Local Ref

data LocalDecl = LocalDecl Type String

data Except = Except Ref Label Label Label


data Stmt = S_breakpoint
          | S_assign Variable Value
          | S_enterMonitor Im
          | S_exitMonitor  Im
          | S_goto Label
          | S_if Expression Label   -- Only condition expressions are allowed
          | S_invoke Expression
          | S_lookupSwitch Label [(Int, Label)]
          | S_nop
          | S_ret Local
          | S_return Im
          | S_returnVoid
          | S_tableSwitch Im Label [(Int, Label)]
          | S_throw Im


data Value = VConst Constant
           | VLocal Local
           | VExpr  Expression

data Label = Label String

data Im = IConst Constant
        | ILocal Local

data Local = Local String
data Constant = C_double Double
              | C_float  Double
              | C_int    Integer
              | C_long   Integer
              | C_string B.ByteString
              | C_null

data Variable = VarRef Ref | VarLocal Local

data RValue = RV_ref   Ref
            | RV_const Constant
            | RV_expr  Expression
            | RV_local Local
            | RV_nnsa  Integer -- TODO: next_next_statement_address ??


data Ref = R_caughtException
         | R_parameter     Integer
         | R_this
         | R_array         Integer
         | R_instanceField Integer
         | R_staticField   Integer


data Expression = E_eq Im Im -- Conditions
                | E_ge Im Im
                | E_le Im Im
                | E_lt Im Im
                | E_ne Im Im
                | E_gt Im Im

                | E_add  Im Im -- Binary ops
                | E_sub  Im Im
                | E_and  Im Im
                | E_or   Im Im
                | E_xor  Im Im
                | E_shl  Im Im
                | E_shr  Im Im
                | E_ushl Im Im
                | E_ushr Im Im
                | E_cmp  Im Im
                | E_cmpg Im Im
                | E_cmpl Im Im
                | E_mul Im Im
                | E_div Im Im
                | E_rem Im Im

                | E_invoke Invoke -- Other
                | E_cast   Type Im
                | E_instanceOf Im Ref
                | E_newArray Type Im
                | E_new Ref
                | E_newMultiArray Type Im [Im] -- TODO: empty dims?

data Invoke = I_interface Im MethodSignature [Im]
            | I_speciale  Im MethodSignature [Im]
            | I_virtual   Im MethodSignature [Im]
            | I_static MethodSignature [Im]

data MethodSignature = MethodSig String [Type] Type

data Type = T_int | T_long | T_float | T_double | T_ref | T_addr | T_void