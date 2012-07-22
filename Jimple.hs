module Jimple where

import qualified Data.ByteString.Char8 as B

import Debug.Trace

import Numeric
import Data.Char
import qualified Data.Map as M

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec as P

import Control.Monad
import Control.Monad.State as ST
import Control.Monad.Reader as R
import Control.Applicative

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
          | S_enterMonitor Im
          | S_exitMonitor  Im
          | S_goto Label
          | S_if Expression Label   -- Only condition expressions are allowed
          | S_invoke InvokeType MethodSignature [Im]
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

data Label = Label Integer
           deriving Show

data Im = IConst Constant
        | ILocal Variable
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
         | R_array         Im Im
         | R_instanceField Im CF.Desc
         | R_staticField      CF.Desc
         | R_object        CF.Class
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

                | E_length Im
                | E_cast   Type Im
                | E_instanceOf Im Ref
                | E_newArray Type Im
                | E_new Ref
                | E_newMultiArray Type Im [Im] -- TODO: empty dims?
                deriving Show

data InvokeType = I_interface Im
                | I_special   Im
                | I_virtual   Im
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
          deriving Show



typeP :: Parser Type
typeP = do
  tag <- anyChar
  case tag of
    'B' -> return T_byte
    'C' -> return T_char
    'D' -> return T_double
    'F' -> return T_float
    'I' -> return T_int
    'J' -> return T_long
    'S' -> return T_short
    'Z' -> return T_boolean
    'L' -> T_object <$> anyChar `manyTill` char ';'
    '[' -> T_array  <$> typeP
    _   -> fail $ "Unknown type tag: " ++ show tag

methodSigP :: ([Type] -> Type -> MethodSignature) -> Parser MethodSignature
methodSigP meth = liftM2 meth paramsP resultP
  where
    paramsP = between (char '(') (char ')') $ P.many $ try typeP
    resultP = choice [try typeP, try voidP]
    voidP = char 'V' >> return T_void

methodSig bs meth = runP (methodSigP meth) () "methodSig" bs

methodSig' bs meth = either (error $ "methodSig: " ++ show bs) id $
                     methodSig bs meth


data JimpleST = JimpleST { jimpleFree  :: [Variable]
                         , jimpleStack :: [Variable] }

byteCodeP = do
  mcode <- optionMaybe anyChar
  case mcode of
    Nothing   -> return ()
    Just code -> parse (ord code) >> byteCodeP

  where
    parse code = case trace ("0x" ++ showHex code "") code of
       -- NOP, needed to maintain correct line count for goto
      0x00 -> append S_nop

       -- @null@
      0x01 -> void $ push $! VConst C_null

      -- int constants -1 to 5
      _ | code `elem` [0x02..0x08] ->
        void $ push $! VConst $! C_int $! fromIntegral $! code - 3

      -- int value from local variable 0 to 3
      _ | code `elem` [0x1a..0x1d] ->
        void $ pushL $! VarLocal $! Local $! 'l' : show (code - 0x1a)

      -- object ref from local variable 0 to 3
      _ | code `elem` [0x2a..0x2d] ->
        void $ pushL $! getLocal $! code - 0x2a

      -- array retrievel
      _ | code `elem` [0x2e..0x35] ->
        arrayGet $ types !! (code - 0x2e)

      -- store int value from stack in local variable 0 to 3
      _ | code `elem` [0x3b..0x3e] -> do
        val <- pop
        append $! S_assign (getLocal $! code - 0x3b) $! VLocal val

      -- array assignment
      _ | code `elem` [0x4f..0x56] ->
        arraySet $ types !! (code - 0x4f)

      -- pop and pop2
      0x57 -> void pop
      0x58 -> replicateM_ 2 pop

      -- dup: a -> a, a
      0x59 -> mapM_ pushL =<< replicate 2 <$> pop

      -- swap: a, b -> b, a
      0x5f -> mapM_ pushL =<< replicateM 2 pop

      -- add two ints
      0x60 -> do (a, b) <- pop2
                 void $ push $! VExpr $! E_add (ILocal a) (ILocal b)

      -- iinc
      0x84 -> do idx <- u1
                 val <- u1
                 append $! S_assign (getLocal idx) $! VExpr $!
                   E_add (ILocal $! getLocal idx) $! IConst $! C_int val

      -- if_icmp
      _ | code `elem` [0x9f..0xa4] ->
        if2 $ [E_eq, E_ne, E_lt, E_ge, E_gt, E_le] !! (code - 0x9f)

      -- goto
      0xa7 -> append =<< S_goto <$> label2

      -- areturn
      0xb0 -> append =<< S_return . ILocal <$> pop

      -- return void
      0xb1 -> append $! S_returnVoid

      -- get instance field
      0xb4 -> do
        Just (CF.FieldRef cs desc) <- askCP
        obj <- pop
        void $ push $! VLocal $! VarRef $! R_instanceField (ILocal obj) desc

      -- invoke special
      0xb7 -> do method <- methodP
                 objRef <- popI
                 params <- replicateM (length $ methodParams method) popI
                 append $! S_invoke (I_special objRef) method params

      -- new object ref
      0xbb -> do Just (CF.ClassRef path) <- askCP
                 void $ push $! VExpr $! E_new $! R_object path

      -- array length
      0xbe -> void . push =<< VExpr . E_length <$> popI


      -- my head just exploded
      _ -> fail $ "Unknown code: 0x" ++ showHex code ""


    getLocal idx = VarLocal $! Local $! 'l' : show idx

    -- pop a value from the stack (return first stack variable)
    pop = do
      (x:xs) <- ST.gets $ jimpleStack . snd
      ST.modify $ \(m, j) -> (m, j { jimpleStack = xs })
      return x

    -- pop as immediate value
    popI = ILocal <$> pop

    -- pop two values
    pop2 = do
      a <- pop
      b <- pop
      return $! (a, b)

    -- push value to stack (assign to next stack variable)
    push v = do
      (x:xs) <- ST.gets $ jimpleFree . snd
      ST.modify $ \(m, j) -> (m, j { jimpleStack = x : jimpleStack j
                                   , jimpleFree  = xs                })
      append $! S_assign x $! v
      return x

    -- push a local variable to stack
    pushL = push . VLocal

    -- append a label-less statement to code
    append cmd =
      ST.modify $ \(m, l) ->
        (m { methodStmts = methodStmts m ++ [(Nothing, cmd)] }, l)

    -- read 1-byte int
    u1 = (fromIntegral . ord) <$> anyChar

    -- read 2-byte int
    u2 = do a <- u1
            b <- u1
            return $! a * 2^8 + b

    -- read 2-byte label (signed short)
    label2 = Label . CF.makeSigned 16 <$> u2

    -- retrieve an element from the constant pool
    askCP = do
      idx <- u2
      M.lookup idx <$> R.asks CF.classConstants

    -- read a method description from constant pool
    methodP = do
      Just (CF.Method path (CF.Desc name tpe)) <- askCP
      return $! methodSig' tpe $! MethodSig path name

    -- general version of if for binary op
    if2 op = do
      a <- popI
      b <- popI
      lbl <- label2
      append $! S_if (a `op` b) lbl

    -- array retrieval
    arrayGet tpe = do
      arr <- popI
      idx <- popI
      void $! push $! VLocal $! VarRef $! R_array arr idx

    -- array retrieval
    arraySet tpe = do
      arr <- popI
      idx <- popI
      val <- pop
      void $! append $! S_assign (VarRef $! R_array arr idx) $! VLocal val



    types = [ T_int,       T_long,    T_float, T_double
            , T_object "", T_boolean, T_char,  T_short  ]


parseJimple :: CF.ClassFile -> B.ByteString -> (Maybe ParseError, JimpleMethod)
parseJimple cf bs =
  go $! ST.runState (R.runReaderT (runPT byteCodeP () "" bs) cf)
        (Method [] [] [] [],
         JimpleST stackVars [])
  where
    stackVars = map (VarLocal . Local . ("s"++) . show) [1..]

    go (Left err, (meth, jst)) = (Just err, meth)
    go (Right _,  (meth, jst)) = (Nothing,  meth)


