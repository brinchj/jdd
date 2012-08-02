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

import Jimple.Types


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
    '[' -> do
      dims <- length <$> (option [] $ many1 $ char '[')
      T_array (dims + 1) <$> typeP
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


data JimpleST = JimpleST { jimpleFree  :: [Variable Value]
                         , jimpleStack :: [Variable Value]
                         , thisPos     :: Integer
                         , prevPos     :: Integer
                         }

byteCodeP = do
  maxStack <- u2
  maxLocals <- u2
  codeLength <- u4
  ST.modify $ \(m, j) -> (m, j { thisPos = 0, prevPos = 0 })
  go codeLength

  where
    go len = do
      pos <- ST.gets $ thisPos . snd
      ST.modify $ \(s, j) -> (s, j { prevPos = pos })
      unless (pos >= len) $
        do mcode <- optionMaybe nextByte
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

      -- ?STORE_#: store int value from stack in local variable 0 to 3
      _ | code `elem` [0x3b..0x4e] ->
        append =<< S_assign (getLocal var) . VLocal <$> pop
        where
          val = code - 0x3b
          var = val `mod` 4
          tpe = types !! (val `div` 4) -- int to object ref

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
      0x60 -> void $ push =<< VExpr <$> apply2 E_add

      -- ISUB: sub two ints
      0x64 -> void $ push =<< VExpr <$> apply2 E_sub

      -- IREM: rem two ints
      0x70 -> void $ push =<< VExpr <$> apply2 E_rem

      -- IAND: and two ints
      0x7e -> void $ push =<< VExpr <$> apply2 E_and

      -- IINC: increment by constant
      0x84 -> do (idx, val) <- liftM2 (,) u1 u1
                 append $! S_assign (getLocal idx) $! VExpr $!
                   E_add (VLocal $! getLocal idx) $! VConst $! C_int val

      -- ?2?: convert types
      _ | code `elem` [0x85..0x93] ->
        void $ push . VLocal =<< pop

      -- IF??: int cmp with zero, eq to le
      _ | code `elem` [0x99..0x9e] ->
        ifz $[E_eq, E_ne, E_lt, E_ge, E_gt, E_le] !! (code - 0x99)

      -- IF_ICMP??: int cmp, eq to le
      _ | code `elem` [0x9f..0xa4] ->
        if2 $ [E_eq, E_ne, E_lt, E_ge, E_gt, E_le] !! (code - 0x9f)

      -- GOTO: unconditional jump
      0xa7 -> append =<< S_goto <$> label2

      -- IRETURN: return int value from stack
      0xac -> append =<< S_return . VLocal <$> pop

      -- ARETURN: return object ref from stack
      0xb0 -> append =<< S_return . VLocal <$> pop

      -- RETURN: return void
      0xb1 -> append $! S_returnVoid

      -- GETFIELD: get instance field
      0xb4 -> do
        Just (CF.FieldRef cs desc) <- askCP2
        obj <- popI
        void $ push $! VLocal $! VarRef $! R_instanceField obj desc

      -- PUTFIELD: get instance field
      0xb5 -> do
        Just (CF.FieldRef cs desc) <- askCP2
        (val, obj) <- liftM2 (,) pop popI
        append $! S_assign (VarRef $! R_instanceField obj desc) $!
                  VLocal val

      -- INVOKEVIRTUAL: invoke instance method on object ref
      0xb6 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 objRef <- popI
                 v      <- getFree
                 append $! S_invoke (I_virtual objRef) method params v
                 void $ push $! VLocal v

      -- INVOKESPECIAL: invoke instance method on object ref
      0xb7 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 objRef <- popI
                 v      <- getFree
                 append $! S_invoke (I_special objRef) method params v
                 void $ push $! VLocal v

      -- INVOKESTATIC: invoke a static method (no object ref)
      0xb8 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 v      <- getFree
                 append $! S_invoke I_static method params v
                 void $ push $! VLocal v

      -- NEW: new object ref
      0xbb -> do Just (CF.ClassRef path) <- askCP2
                 void $ push $! VExpr $! E_new $! R_object path

      -- NEWARRAY: new array of primitive type
      0xbc -> do tpe   <- fromIntegral <$> u1
                 count <- popI
                 void $ push $ VExpr $! E_newArray (atypes !! (tpe - 4)) count

      -- ARRAYLENGTH: get length of array ref
      0xbe -> void . push =<< VExpr . E_length <$> popI

      -- IFNULL: if value is null jump
      0xc6 -> do v <- popI
                 append =<< S_if (E_eq v $ VConst C_null) <$> label2

      -- IFNONNULL: if value is null jump
      0xc7 -> do v <- popI
                 append =<< S_if (E_ne v $ VConst C_null) <$> label2

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
    popI = VLocal <$> pop

    -- get free stack variable
    getFree = do
      (x:xs) <- ST.gets $ jimpleFree . snd
      ST.modify $ \(m, j) -> (m, j { jimpleStack = x : jimpleStack j
                                   , jimpleFree  = xs                })
      return x

    -- push value to stack (assign to next stack variable)
    push v = do
      x <- getFree
      append $! S_assign x v
      return x

    -- push a local variable to stack
    pushL = push . VLocal

    -- append a label-less statement to code
    append cmd = do
      pos <- ST.gets $ prevPos . snd
      ST.modify $ \(m, l) ->
        (m { methodStmts = methodStmts m ++ [(Just $ Label pos, cmd)] }, l)

    -- read and register 1 byte
    nextByte = do b <- anyChar
                  ST.modify $ \(m, j) -> (m, j { thisPos = 1 + thisPos j })
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

    -- apply operator to stack vars
    apply1 op = liftM op popI
    apply2 op = liftM2 (flip op) popI popI

    -- general version of if for cmp with zero
    ifz op = append =<< liftM2 S_if (apply1 $ flip op $ VConst $ C_int 0) label2

    -- general version of if for binary op
    if2 op = append =<< liftM2 S_if (apply2 op) label2

    -- array retrieval
    arrayGet tpe =
      void . push =<< VLocal . VarRef <$> apply2 R_array

    -- array retrieval
    arraySet tpe = do
      ref <- VarRef <$> apply2 R_array
      void . append =<< S_assign ref . VLocal <$> pop


    -- Convert constant pool value to VConst
    cpToVC (CF.Str s) = VConst $! C_string s
    cpToVC a = error $ "Unknown constant: " ++ show a

    types = [ T_int,       T_long,    T_float, T_double
            , T_object "", T_boolean, T_char,  T_short  ]

    atypes = [ T_boolean,  T_char  , T_float, T_double
             , T_byte   ,  T_short , T_int  , T_long   ]

parseJimple :: CF.ClassFile -> B.ByteString -> (Maybe ParseError, JimpleMethod Value)
parseJimple cf bs =
  go $! ST.runState (R.runReaderT (runPT byteCodeP () "" bs) cf)
        (Method [] [] [] [],
         JimpleST stackVars [] 0 0)
  where
    stackVars = map (VarLocal . Local . ("s"++) . show) [1..]

    go (Left err, (meth, jst)) = (Just err, meth)
    go (Right _,  (meth, jst)) = (Nothing,  meth)


