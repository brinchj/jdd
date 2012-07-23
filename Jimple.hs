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
                         , jimpleStack :: [Variable]
                         , bytePos     :: Integer
                         }

byteCodeP = do
  maxStack <- u2
  maxLocals <- u2
  codeLength <- u4
  ST.modify $ \(m, j) -> (m, j { bytePos = 0 })
  go codeLength

  where
    go len = do
      pos <- ST.gets $ bytePos . snd
      if pos >= len then return () else do
        mcode <- optionMaybe nextByte
        case mcode of
          Nothing   -> return ()
          Just code -> parse (ord code) >> go len

    parse code = case trace ("0x" ++ showHex code "") code of
       -- NOP: needed to maintain correct line count for goto
      0x00 -> append S_nop

       -- ACONST_NULL: @null@
      0x01 -> void $ push $! VConst C_null

      -- ICONST_#: constants -1 to 5
      _ | code `elem` [0x02..0x08] ->
        void $ push $! VConst $! C_int $! fromIntegral $! code - 3

      -- LCONST_#: long constants 0L to 1L
      0x09 -> void $ push $! VConst $! C_long 0
      0x0a -> void $ push $! VConst $! C_long 1

      -- FCONST_#: float constants 0.0f to 2.0f
      _ | code `elem` [0x0b, 0x0c, 0x0d] ->
        void $ push $! VConst $! C_float $! fromIntegral $! code - 0x0b

      -- DCONST_#: double constants 0.0 to 1.0
      0x0e -> void $ push $! VConst $! C_double 0.0
      0x0f -> void $ push $! VConst $! C_double 1.0

      -- BIPUSH: push byte to stack as int
      0x10 -> void . push =<< VConst . C_int <$> u1

      -- SIPUSH: signed short to stack as int
      0x11 -> void . push =<< VConst . C_int <$> s2

      -- LDC#: push from constant pool (String, int, float) + wide / double
      -- TODO: Add support for other types than String (Str)
      0x12 -> do Just cpC <- askCP1
                 void $ push $! cpToVC cpC
      0x13 -> do Just cpC <- askCP2
                 void $ push $! cpToVC cpC
      0x14 -> do Just cpC <- askCP2
                 void $ push $! cpToVC cpC

      -- ?LOAD: load value from local variable, int to object ref
      _ | code `elem` [0x15..0x19] -> void . pushL =<< getLocal <$> u1

      -- ?LOAD_#: int to object ref value from local variable 0 to 3
      _ | code `elem` [0x1a..0x2d] -> void $ pushL $! getLocal var
        where
          val = code - 0x1a
          var = val `mod` 4
          tpe = types !! (val `div` 4) -- int to object ref

      -- ?ALOAD: array retrieval, int to short
      _ | code `elem` [0x2e..0x35] -> arrayGet $ types !! (code - 0x2e)

      -- ?STORE: store value in local variable #, int to object ref
      _ | code `elem` [0x36..0x3a] -> void . pushL =<< getLocal <$> u1

      -- ISTORE_#: store int value from stack in local variable 0 to 3
      _ | code `elem` [0x3b..0x3e] ->
        append =<< S_assign (getLocal $ code - 0x3b) . VLocal <$> pop

      -- ?ASTORE: array assignment, int to short
      _ | code `elem` [0x4f..0x56] -> arraySet $ types !! (code - 0x4f)

      -- POP and POP2
      0x57 -> void pop
      0x58 -> replicateM_ 2 pop

      -- DUP: a -> a, a
      0x59 -> mapM_ pushL =<< replicate 2 <$> pop

      -- SWAP: a, b -> b, a
      0x5f -> mapM_ pushL =<< replicateM 2 pop

      -- IADD: add two ints
      0x60 -> void $ push =<< VExpr <$> liftM2 E_add popI popI

      -- IINC: increment by constant
      0x84 -> do (idx, val) <- liftM2 (,) u1 u1
                 append $! S_assign (getLocal idx) $! VExpr $!
                   E_add (ILocal $! getLocal idx) $! IConst $! C_int val

      -- IF_ICMP??: int cmp, eq to le
      _ | code `elem` [0x9f..0xa4] ->
        if2 $ [E_eq, E_ne, E_lt, E_ge, E_gt, E_le] !! (code - 0x9f)

      -- GOTO: unconditional jump
      0xa7 -> append =<< S_goto <$> label2

      -- IRETURN: return int value from stack
      0xac -> append =<< S_return . ILocal <$> pop

      -- ARETURN: return object ref from stack
      0xb0 -> append =<< S_return . ILocal <$> pop

      -- RETURN: return void
      0xb1 -> append $! S_returnVoid

      -- GETFIELD: get instance field
      0xb4 -> do
        Just (CF.FieldRef cs desc) <- askCP2
        obj <- popI
        void $ push $! VLocal $! VarRef $! R_instanceField obj desc

      -- INVOKESPECIAL: invoke instance method on object ref
      0xb7 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 objRef <- popI
                 append $! S_invoke (I_special objRef) method params

      -- NEW: new object ref
      0xbb -> do Just (CF.ClassRef path) <- askCP2
                 void $ push $! VExpr $! E_new $! R_object path

      -- ARRAYLENGTH: get length of array ref
      0xbe -> void . push =<< VExpr . E_length <$> popI

      -- UNASSIGNED: skip (can appear after last return; garbage)
      _ | code `elem` [0xcb..0xfd] -> return ()

      -- NOT IMPLEMENTED: my head just exploded
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
    pop2 = liftM2 (,) pop pop

    -- push value to stack (assign to next stack variable)
    push v = do
      (x:xs) <- ST.gets $ jimpleFree . snd
      ST.modify $ \(m, j) -> (m, j { jimpleStack = x : jimpleStack j
                                   , jimpleFree  = xs                })
      append $! S_assign x v
      return x

    -- push a local variable to stack
    pushL = push . VLocal

    -- append a label-less statement to code
    append cmd = do
      pos <- ST.gets $ bytePos . snd
      ST.modify $ \(m, l) ->
        (m { methodStmts = methodStmts m ++ [(Just $ Label pos, cmd)] }, l)

    -- read and register 1 byte
    nextByte = do b <- anyChar
                  ST.modify $ \(m, j) -> (m, j { bytePos = 1 + bytePos j })
                  return b

    -- read 1-byte int
    u1 = (fromIntegral . ord) <$> nextByte

    -- read 2-byte int
    u2 = do (a, b) <- liftM2 (,) u1 u1
            return $! a * 2^8 + b

    -- read 4-byte int
    u4 = do (a, b) <- liftM2 (,) u2 u2
            return $! a * 2^16 + b

    -- read 2-byte signed int
    s2 = CF.makeSigned 16 <$> u2

    -- read 2-byte label (signed short)
    label2 = Label <$> s2

    -- retrieve an element from the constant pool
    askCP u = liftM2 M.lookup u $ R.asks CF.classConstants
    askCP1 = askCP u1
    askCP2 = askCP u2

    -- read a method description from constant pool
    methodP = do
      Just (CF.Method path (CF.Desc name tpe)) <- askCP2
      return $! methodSig' tpe $! MethodSig path name

    -- general version of if for binary op
    if2 op = do
      con <- liftM2 (flip op) popI popI
      append =<< S_if con <$> label2

    -- array retrieval
    arrayGet tpe =
      void . push =<< VLocal . VarRef <$> liftM2 (flip R_array) popI popI

    -- array retrieval
    arraySet tpe = do
      ref <- VarRef <$> liftM2 (flip R_array) popI popI
      void . append =<< S_assign ref . VLocal <$> pop


    -- Convert constant pool value to VConst
    cpToVC (CF.Str s) = VConst $! C_string s
    cpToVC a = error $ "Unknown constant: " ++ show a

    types = [ T_int,       T_long,    T_float, T_double
            , T_object "", T_boolean, T_char,  T_short  ]


parseJimple :: CF.ClassFile -> B.ByteString -> (Maybe ParseError, JimpleMethod)
parseJimple cf bs =
  go $! ST.runState (R.runReaderT (runPT byteCodeP () "" bs) cf)
        (Method [] [] [] [],
         JimpleST stackVars [] 0)
  where
    stackVars = map (VarLocal . Local . ("s"++) . show) [1..]

    go (Left err, (meth, jst)) = (Just err, meth)
    go (Right _,  (meth, jst)) = (Nothing,  meth)


