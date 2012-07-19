{-# LANGUAGE RecordWildCards #-}

module Jimple where

import qualified Data.ByteString.Char8 as B


import Data.Char

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec

import Control.Monad
import Control.Monad.State as ST
import Control.Applicative

import Parser (parseClassFile)



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
          deriving Show


data Value = VConst Constant
           | VLocal Variable
           | VExpr  Expression
           deriving Show

data Label = Label String
           deriving Show

data Im = IConst Constant
        | ILocal Local
        deriving Show

data Local = Local String
             deriving Show

data Constant = C_double Double
              | C_float  Double
              | C_int    Integer
              | C_long   Integer
              | C_string B.ByteString
              | C_null
              deriving Show

data Variable = VarRef Ref | VarLocal Local
              deriving Show

data RValue = RV_ref   Ref
            | RV_const Constant
            | RV_expr  Expression
            | RV_local Local
            | RV_nnsa  Integer -- TODO: next_next_statement_address ??
            deriving Show


data Ref = R_caughtException
         | R_parameter     Integer
         | R_this
         | R_array         Integer
         | R_instanceField Integer
         | R_staticField   Integer
         deriving Show


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
                deriving Show

data Invoke = I_interface Im MethodSignature [Im]
            | I_speciale  Im MethodSignature [Im]
            | I_virtual   Im MethodSignature [Im]
            | I_static MethodSignature [Im]
            deriving Show

data MethodSignature = MethodSig String [Type] Type
                     deriving Show

data Type = T_int | T_long | T_float | T_double | T_ref | T_addr | T_void
          deriving Show



byteCodeP = do
  code <- anyToken
  case code of
    '\0' -> byteCodeP -- NOP
    '\1' -> -- @null@
      do v <- getStackVar
         append $ S_assign v $ VConst C_null
         byteCodeP
    _ | code <= '\8' -> -- int constants -1 to 5
      do v <- getStackVar
         append $ S_assign v $ VConst $
           C_int $ fromIntegral $ ord code - 3
         byteCodeP
    '\42' -> -- object ref from local variable 0
      do sv <- getStackVar
         append $ S_assign sv $ VLocal $ VarLocal $ Local "l0"
         byteCodeP

    _ -> error $ "Unknown code: " ++ (show $ ord code)
  where
    getStackVar = getVar "s"
    -- getLocalVar = getVar "l"
    getVar p = do
      x:xs <- gets snd
      ST.modify $ \(m, _) -> (m, xs)
      return $ VarLocal $ Local $ p ++ show x

    append cmd = do
      ST.modify $ \(m, l) ->
        (m { methodStmts = methodStmts m ++ [(Nothing, cmd)] }, l)

parseJimple cf bs = fst $ ST.execState (runPT byteCodeP () "" bs)
                    (Method [] [] [] [], [1..])


test = do
  cf <- parseClassFile <$> B.readFile "aa.class"
  print cf
  print $ parseJimple cf "\NUL\SOH\NUL\SOH\NUL\NUL\NUL\ENQ*\183\NUL\b\177\NUL\NUL\NUL\NUL"