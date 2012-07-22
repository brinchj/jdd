module Jimple where

import qualified Data.ByteString.Char8 as B

import Debug.Trace
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
    parse code = case traceShow code code of
       -- NOP, needed to maintain correct line count for goto
      0 -> append S_nop

       -- @null@
      1 -> void $ push $! VConst C_null

      -- int constants -1 to 5
      _ | code `elem` [2..8] ->
        void $ push $! VConst $! C_int $! fromIntegral $! code - 3

      -- int value from local variable 0 to 3
      _ | code `elem` [26..29] ->
        void $ pushL $! VarLocal $! Local $! 'l' : show (code - 26)

      -- object ref from local variable 0 to 3
      _ | code `elem` [42..45] ->
        void $ pushL $! VarLocal $! Local $! 'l' : show (code - 42)

      -- array assignment
      _ | code `elem` [46..53] ->
        arraySet $ [ T_int, T_long, T_float, T_double
                   , T_object "", T_boolean, T_char, T_short ] !! (code - 46)

      -- store int value from stack in local variable 0 to 3
      _ | code `elem` [59..62] -> do
        val <- pop
        append $! S_assign (VarLocal $! Local $! 'l' : show (code - 59))
          $! VLocal val

      -- pop and pop2
      87 -> void pop
      88 -> replicateM_ 2 pop

      -- dup: a -> a, a
      89 -> mapM_ pushL =<< replicate 2 <$> pop

      -- swap: a, b -> b, a
      95 -> mapM_ pushL =<< replicateM 2 pop

      -- add two ints
      96 -> do (a, b) <- pop2
               void $ push $! VExpr $! E_add (ILocal a) (ILocal b)

      -- if_icmp
      _ | code `elem` [159..164] ->
        if2 $ [E_eq, E_ne, E_lt, E_ge, E_gt, E_le] !! (code - 159)

      -- areturn
      176 -> do obj <- pop
                append $! S_return $! ILocal obj

      -- return void
      177 -> append $! S_returnVoid

      -- get field
      180 -> do
        Just (CF.FieldRef cs desc) <- askCP
        obj <- pop
        void $ push $! VLocal $! VarRef $! R_instanceField (ILocal obj) desc

      -- invoke special
      183 -> do method <- methodP
                objRef <- popI
                params <- replicateM (length $ methodParams method) popI
                append $! S_invoke (I_special objRef) method params

      -- new object ref
      187 -> do Just (CF.ClassRef path) <- askCP
                void $ push $! VExpr $! E_new $! R_object path

      -- array length
      190 -> do ref <- popI
                void $ push $! VExpr $! E_length ref


      -- my head just exploded
      _ -> fail $ "Unknown code: " ++ show code


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
            return $! a * 8 + b


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
      offset <- u2
      append $! S_if (a `op` b) $! Label offset

    -- array assignment
    arraySet tpe = do
      arr <- popI
      idx <- popI
      void $! push $! VLocal $! VarRef $! R_array arr idx



parseJimple :: CF.ClassFile -> B.ByteString -> (Maybe ParseError, JimpleMethod)
parseJimple cf bs =
  go $! ST.runState (R.runReaderT (runPT byteCodeP () "" bs) cf)
        (Method [] [] [] [],
         JimpleST stackVars [])
  where
    stackVars = map (VarLocal . Local . ("s"++) . show) [1..]

    go (Left err, (meth, jst)) = (Just err, meth)
    go (Right _,  (meth, jst)) = (Nothing,  meth)


