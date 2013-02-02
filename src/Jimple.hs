module Jimple where

import Prelude ()
import CustomPrelude

import qualified Data.ByteString.Char8 as B

import Data.Bits
import Data.Char

import Numeric

import qualified Data.Text as T

import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Data.List     as L

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec as P

import Control.Monad.State as ST
import Control.Monad.Reader as R

import qualified Parser as CF

import Jimple.Types
import Jimple.Exceptions
import Jimple.Rewrite (try_)


typeP :: Parser Type
typeP = P.try $ do
  tag <- anyChar
  case tag of
    'B' -> return TByte
    'C' -> return TChar
    'D' -> return TDouble
    'F' -> return TFloat
    'I' -> return TInt
    'J' -> return TLong
    'S' -> return TShort
    'Z' -> return TBoolean
    'V' -> return TVoid
    'L' -> TObject . T.pack <$> anyChar `manyTill` char ';'
    '[' -> do
      dims <- length <$> option [] (many1 $ char '[')
      TArray (dims + 1) <$> typeP
    _   -> failT $ "Unknown type tag: " ++ showT tag

methodTypeP :: Parser ([Type], Type)
methodTypeP = do
  params <- P.between (char '(') (char ')') $ P.optionMaybe $ P.many typeP
  result <- typeP
  return (fromMaybe [] params, result)


methodTypeFromBS :: B.ByteString -> Either ParseError ([Type], Type)
methodTypeFromBS = runP methodTypeP () "typesFromBS"

methodTypeFromBS' :: B.ByteString -> ([Type], Type)
methodTypeFromBS' = either (errorT . showT) id . methodTypeFromBS


-- | Parse Type from ByteString
-- >>> typeFromBS (B.pack "I")
-- Right TInt
-- >>> typeFromBS (B.pack "[[D]]")
-- Right (TArray 2 TDouble)
typeFromBS :: B.ByteString -> Either ParseError Type
typeFromBS = runP typeP () "typeFromBS"


-- | Parse Type from ByteString and return TUnknown on error
-- >>> typeFromBS' (B.pack "I")
-- TInt
-- >>> typeFromBS' B.empty
-- TUnknown
typeFromBS' :: B.ByteString -> Type
typeFromBS' = either (const TUnknown) id . typeFromBS


methodSigP :: ([Type] -> Type -> MethodSignature) -> Parser MethodSignature
methodSigP meth = liftM2 meth paramsP resultP
  where
    paramsP = between (char '(') (char ')') $ P.many $ P.try typeP
    resultP = choice [P.try typeP, P.try voidP]
    voidP = char 'V' >> return TVoid


methodSigFromBS :: ByteString -> ([Type] -> Type -> MethodSignature) ->
                   Either ParseError MethodSignature
methodSigFromBS bs meth = runP (methodSigP meth) () "methodSig" bs

methodSigFromBS' :: ByteString -> ([Type] -> Type -> MethodSignature) ->
                    MethodSignature
methodSigFromBS' bs meth = either (errorT $ "methodSig: " ++ showT bs) id $
                     methodSigFromBS bs meth


-- | Convert a String to an unsigned integer
-- >>> bytesToUnsigned ""
-- 0
-- >>> bytesToUnsigned "\255"
-- 255
-- >>> bytesToUnsigned "\1\0"
-- 256
-- >>> bytesToUnsigned "1337"
-- 825439031
bytesToUnsigned :: String -> Integer
bytesToUnsigned = L.foldl' (\n b -> n * 256 + fromIntegral (ord b)) 0


exceptionTableM :: Parser ExceptTable
exceptionTableM = do
  size <- u2
  entries <- replicateM (fromIntegral size) entry
  return $ ExceptTable entries
  where
    u2 = bytesToUnsigned <$> count 2 anyToken
    entry = do
      (from, to) <- liftM2 (,) u2 u2
      target <- u2
      eid <- u2
      return $ ExceptEntry from to target eid


data JimpleST = JimpleST { jimpleFree  :: [Variable Value]
                         , jimpleStack :: [Variable Value]
                         , jimpleScope :: [( Integer
                                           , [Variable Value], [Variable Value])]
                         , thisPos     :: Integer
                         , prevPos     :: Integer
                         }

byteCodeP excTable = do
  ST.modify $ \(m, j) -> (m, j { thisPos = 0, prevPos = 0 })
  codeM

  where
    codeM = do
      pos <- ST.gets $ thisPos . snd
      modifySnd $ \j -> j { prevPos = pos }

      -- Check for end-of-scope
      revertScopes pos

      -- Handle try
      F.forM_ (fromStart excTable pos) $ \exc ->
        append $ STry (pos, exceptTo exc) $ exceptTarget exc

      -- Handle catch
      F.forM_ (fromTarget excTable pos) tryCatch

      -- Check for EOF
      mcode <- optionMaybe nextByte
      case mcode of
        Nothing -> return ()
        Just c  -> parseByte (ord c) >> codeM

    revertScopes pos = do
      scope <- ST.gets $ headMay . jimpleScope . snd
      case scope of
        Just (endPos, free, stack) | endPos <= pos -> do
          -- error $ showT (endPos, pos)
          modifySnd $ \j -> j { jimpleFree = free, jimpleStack = stack
                              , jimpleScope = tail $ jimpleScope j }
          revertScopes pos
        _ -> return ()


    tryCatch (ee@(ExceptEntry start to _ eid)) = do
      mx <- if eid == 0 then return Nothing else getCP eid
      let mr = (\(CF.ClassRef ref) -> ref) `fmap` mx
      append $ SCatch (start, to) ee mr
      void $ pushL $! VarLocal $! Local "exc"


    parseByte code = case code of
      -- NOP: needed to maintain correct line count for goto
      0x00 -> append SNop

      -- ACONST_NULL: @null@
      0x01 -> void $ push $! VConst CNull

      -- ICONST_#: constants -1 to 5
      _ | code `elem` [0x02..0x08] ->
        void $ push $! VConst $! CInt $! fromIntegral $! code - 3

      -- LCONST_#: long constants 0L to 1L
      0x09 -> void $ push $! VConst $! CLong 0
      0x0a -> void $ push $! VConst $! CLong 1

      -- FCONST_#: float constants 0.0f to 2.0f
      _ | code `elem` [0x0b, 0x0c, 0x0d] ->
        void $ push $! VConst $! CFloat $! fromIntegral $! code - 0x0b

      -- DCONST_#: double constants 0.0 to 1.0
      0x0e -> void $ push $! VConst $! CDouble 0.0
      0x0f -> void $ push $! VConst $! CDouble 1.0

      -- BIPUSH: signed byte to stack as int
      0x10 -> void . push =<< VConst . CInt <$> s1

      -- SIPUSH: signed short to stack as int
      0x11 -> void . push =<< VConst . CInt <$> s2

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
      _ | code `elem` [0x36..0x3a] -> do
        var <- u1
        append =<< SAssign (getLocal var) . VLocal <$> pop


      -- ?STORE_#: store int value from stack in local variable 0 to 3
      _ | code `elem` [0x3b..0x4e] ->
        append =<< SAssign (getLocal var) . VLocal <$> pop
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

      -- DUP_x1: b, a -> a, b, a
      0x5a -> do (a, b) <- liftM2 (,) pop pop
                 mapM_ pushL [a, b, a]

      -- TODO: Alternative forms
      -- DUP_x2: c, b, a -> a, c, b, a
      0x5b -> do (a, b, c) <- liftM3 (,,) pop pop pop
                 mapM_ pushL [a, c, b, a]

      -- DUP2: b, a -> b, a, b, a
      0x5c -> do (a, b) <- liftM2 (,) pop pop
                 mapM_ pushL [b, a, b, a]

      -- DUP2_x1: c, b, a -> b, a, c, b, a
      0x5d -> do (a, b, c) <- liftM3 (,,) pop pop pop
                 mapM_ pushL [b, a, c, b, a]

      -- DUP2_x2: d, c, b, a -> b, a, d, c, b, a
      0x5e -> do (a, b, c, d) <- liftM4 (,,,) pop pop pop pop
                 mapM_ pushL [b, a, d, c, b, a]

      -- SWAP: a, b -> b, a
      0x5f -> mapM_ pushL =<< replicateM 2 pop

      -- IADD: add two ints
      0x60 -> void $ push =<< VExpr <$> apply2 EAdd

      -- ISUB: sub two ints
      0x64 -> void $ push =<< VExpr <$> apply2 ESub

      -- IMUL: multiply two ints
      0x68 -> void $ push =<< VExpr <$> apply2 EMul

      -- IDIV: divide two ints
      0x6c -> void $ push =<< VExpr <$> apply2 EDiv

      -- IREM: rem two ints
      0x70 -> void $ push =<< VExpr <$> apply2 ERem

      -- IAND: and two ints
      0x7e -> void $ push =<< VExpr <$> apply2 EAnd

      -- IINC: increment by constant
      0x84 -> do (idx, val) <- liftM2 (,) u1 s1
                 append $! SAssign (getLocal idx) $! VExpr $!
                   EAdd (VLocal $! getLocal idx) $! VConst $! CInt val

      -- ?2?: convert types
      _a | code `elem` [0x85..0x93] ->
             void $ push . VLocal =<< pop

      -- IF??: int cmp with zero, eq to le
         | code `elem` [0x99..0x9e] ->
             ifz $[EEq, ENe, ELt, EGe, EGt, ELe] !! (code - 0x99)

      -- IF_ICMP??: int cmp, eq to le
         | code `elem` [0x9f..0xa4] ->
             if2 $ [EEq, ENe, ELt, EGe, EGt, ELe] !! (code - 0x9f)

      -- GOTO: unconditional jump
      0xa7 -> append =<< SGoto <$> label2

      -- LOOKUPSWITCH: switch statement
      0xab -> do
        -- get value for switching
        v <- popI
        -- skip padding
        pos <- fromIntegral <$> thisPos <$> ST.gets snd
        let off = pos `mod` 4
        when (off > 0) $
          replicateM_ (4 - off) u1
        -- address for default code
        defaultByte <- Label <$> s4
        -- match-pairs and their addresses
        npairs <- fromIntegral <$> s4
        pairs <- replicateM npairs $ liftM2 (,) s4 (Label <$> s4)
        -- build lookupSwitch
        append $! SLookupSwitch v defaultByte $ L.sortBy (comparing snd) pairs

      -- IRETURN: return int value from stack
      0xac -> append =<< SReturn . Just . VLocal <$> pop

      -- ARETURN: return object ref from stack
      0xb0 -> append =<< SReturn . Just . VLocal <$> pop

      -- RETURN: return void
      0xb1 -> append $ SReturn Nothing

      -- GETSTATIC: get static field
      0xb2 -> do
        Just (CF.FieldRef cs desc) <- askCP2
        void $ push $! VLocal $! VarRef $! RStaticField cs desc

      -- GETFIELD: get instance field
      0xb4 -> do
        Just (CF.FieldRef _cs desc) <- askCP2
        obj <- popI
        void $ push $! VLocal $! VarRef $! RInstanceField obj desc

      -- PUTFIELD: get instance field
      0xb5 -> do
        Just (CF.FieldRef _cs desc) <- askCP2
        (val, obj) <- liftM2 (,) pop popI
        append $! SAssign (VarRef $! RInstanceField obj desc) $!
                  VLocal val

      -- INVOKEVIRTUAL: invoke instance method on object ref
      0xb6 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 objRef <- popI
                 v      <- resultVar method
                 append $! SAssign v $ VExpr $
                           EInvoke (IVirtual objRef) method params

      -- INVOKESPECIAL: invoke instance method on object ref
      0xb7 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 objRef <- popI
                 v      <- resultVar method
                 append $! SAssign v $ VExpr $
                           EInvoke (ISpecial objRef) method params

      -- INVOKESTATIC: invoke a static method (no object ref)
      0xb8 -> do method <- methodP
                 params <- replicateM (length $ methodParams method) popI
                 v      <- resultVar method
                 append $! SAssign v $ VExpr $
                           EInvoke IStatic method params

      -- ATHROW: throw an exception
      0xbf -> do v <- popI
                 append $ SThrow v

      -- NEW: new object ref
      0xbb -> do Just (CF.ClassRef path) <- askCP2
                 void $ push $! VExpr $! ENew (RObject path) []

      -- NEWARRAY: new array of primitive type
      0xbc -> do tpe   <- fromIntegral <$> u1
                 elems <- popI
                 void $ push $ VExpr $! ENewArray (atypes !! (tpe - 4)) elems

      -- ARRAYLENGTH: get length of array ref
      0xbe -> void . push =<< VExpr . ELength <$> popI

      -- CHECKCAST: cast an object to type
      0xc0 -> do Just (CF.ClassRef (CF.Class path)) <- askCP2
                 obj <- popI
                 void $ push $ VExpr $! ECast (TObject $ fromUtf8 path) obj

      -- IFNULL: if value is null jump
      0xc6 -> do v <- popI
                 append =<< SIf (EEq v $ VConst CNull) <$> label2

      -- IFNONNULL: if value is null jump
      0xc7 -> do v <- popI
                 append =<< SIf (ENe v $ VConst CNull) <$> label2

      -- UNASSIGNED: skip (can appear after last return; garbage)
      _ | code `elem` [0xcb..0xfd] -> return ()

      -- NOT IMPLEMENTED: my head just exploded
      _ -> error $ "Unknown code: 0x" ++ showHex code ""


    getLocal idx = VarLocal $! Local $! 'l' `T.cons` showT idx

    -- pop a value from the stack (return first stack variable)
    pop = do
      (x:xs) <- ST.gets $ jimpleStack . snd
      modifySnd $ \j -> j { jimpleStack = xs }
      return x

    -- pop as immediate value
    popI = VLocal <$> pop

    -- get free stack variable
    getFree = do
      (x:xs) <- ST.gets $ jimpleFree . snd
      modifySnd $ \j -> j { jimpleStack = x : jimpleStack j
                          , jimpleFree  = xs                }
      return x

    -- push value to stack (assign to next stack variable)
    push v = do
      x <- getFree
      append $! SAssign x v
      return x

    -- push a local variable to stack
    pushL = push . VLocal

    -- append a label-less statement to code
    append cmd = do
      pos <- ST.gets $ prevPos . snd
      stmts <- ST.gets $ methodStmts . fst
      -- When injecting a STry{} we also "eat" the following label
      let mlabel = if L.null stmts || not (try_ $ last stmts)
                   then Just $ Label pos
                   else Nothing
      modifyFst $ \m ->
        m { methodStmts = methodStmts m ++ [(mlabel, cmd)] }

    -- read and register 1 byte
    nextByte = do b <- anyChar
                  modifySnd $ \j -> j { thisPos = 1 + thisPos j }
                  return b

    -- read 1-byte int
    u1 = (fromIntegral . ord) <$> nextByte

    -- read 2-byte int
    u2 = bytesToUnsigned <$> count 2 nextByte

    -- read 4-byte int
    u4 = bytesToUnsigned <$> count 4 nextByte

    -- read 1-byte signed int
    s1 = CF.makeSigned  8 <$> u1

    -- read 2-byte signed int
    s2 = CF.makeSigned 16 <$> u2

    -- read 4-byte signed int
    s4 = CF.makeSigned 32 <$> u4

    -- read 2-byte label (signed short)
    label2 = Label <$> s2

    -- retrieve an element from the constant pool
    getCP u = M.lookup u <$> R.asks CF.classConstants
    askCP u = liftM2 M.lookup u $ R.asks CF.classConstants
    askCP1 = askCP u1
    askCP2 = askCP u2

    -- read a method description from constant pool
    methodP = do
      Just (CF.Method path (CF.Desc name tpe)) <- askCP2
      return $! methodSigFromBS' tpe $! MethodSig path (fromUtf8 name) []

    -- apply operator to stack vars
    apply1 op = liftM op popI
    apply2 op = liftM2 (flip op) popI popI

    -- general version of if for cmp with zero
    ifz op = do
      lbl <- label2
      append =<< liftM (flip SIf lbl) (apply1 $ flip op $ VConst $ CInt 0)
      newScope $ fromIntegral lbl

    -- general version of if for binary op
    if2 op = do
      lbl <- label2
      append =<< liftM (flip SIf lbl) (apply2 op)
      newScope $ fromIntegral lbl

    -- create a new scope
    newScope endPos = do
      prevPos <- ST.gets $ prevPos . snd
      free  <- ST.gets $ jimpleFree . snd
      stack <- ST.gets $ jimpleStack . snd
      modifySnd $ \j -> j { jimpleScope = (prevPos + endPos, free, stack) :
                                          jimpleScope j }

    -- array retrieval
    arrayGet tpe =
      void . push =<< VLocal . VarRef <$> apply2 RArray

    -- array retrieval
    arraySet tpe = do
      var <- VLocal <$> pop
      ref <- VarRef <$> apply2 RArray
      append $ SAssign ref var


    -- allocate new variable for result of method call unless it's void
    resultVar m | methodResult m == TVoid = return $ VarLocal $ Local "_"
                | otherwise                = getFree

    -- Convert constant pool value to VConst
    cpToVC (CF.Str s) = VConst $! CString $! fromUtf8 s
    cpToVC a = errorT $ "Unknown constant: " ++ showT a

    types = [ TInt,       TLong,    TFloat, TDouble
            , TObject "", TBoolean, TChar,  TShort  ]

    atypes = [ TBoolean,  TChar  , TFloat, TDouble
             , TByte   ,  TShort , TInt  , TLong   ]

parseJimple :: CF.ClassFile -> B.ByteString -> (Maybe ParseError, JimpleMethod Value)
parseJimple cf method
  | hasCode   = go $! ST.runState (R.runReaderT goM cf) (emptyMethod, emptyState)
  | otherwise = (Nothing, emptyMethod)
  where
    emptyMethod = Method   sig [] [] [] []
    emptyState  = JimpleST stackVars [] [] 0 0

    stackVars = map (VarLocal . Local . ('s' `T.cons`) . showT)
                     [1 :: Int ..]

    go (Left err, (meth, _)) = (Just err, meth)
    go (Right _,  (meth, _)) = (Nothing,  meth)

    CF.AttrBlock{..} = CF.classMethods cf M.! method
    bytes = blockAttrs M.! "Code"
    hasCode = "Code" `M.member` blockAttrs

    goM = do
      let MethodSig _ _ _ vs _r =
            methodSigFromBS' blockDesc $
            MethodSig (CF.Class "") (fromUtf8 blockDesc) []
      modifyFst $ \m -> m { methodLocalDecls = zipWith decl vs ns }
      -- Add reference to this from "l0" when method is not static
      unless isStatic $
        modifyFst $ \m -> m {
          methodStmts =
             [(Nothing,
               SAssign (VarLocal $ Local "l0") (VLocal $ VarRef RThis))]
          }

      let dropSize = 4  -- maxStack and maxLocals

      -- Extract codeLength and then codeBytes
      let codeLengthM = do
            _ <- count dropSize anyToken  -- maxStack, maxLocals
            bytesToUnsigned <$> count 4 anyToken
      let codeLength = either (errorT . showT) id $
                       runP codeLengthM () "codeSize" bytes
      let (codeBytes, rest) = B.splitAt (fromIntegral codeLength) $
                              B.drop (dropSize + 4) bytes

      -- Extract exception table
      let excTable = cleanupExcTable $
                     either (errorT . showT) id $
                     runP exceptionTableM () "exceptionTable" rest

      -- code <- runP (count codeSize anyChar)
      runPT (byteCodeP excTable) () "" codeBytes

    decl t n = LocalDecl t $ Local $ 'l' `T.cons` showT n

    ns = if isStatic then [0 :: Int ..] else [1..]
    isStatic = FStatic `elem` accFlags

    sig = MethodSig (CF.unClassRef $ CF.classThis cf) name accFlags params result

    name = fromUtf8 blockName
    (params, result) = methodTypeFromBS' blockDesc

    accFlags = getFlags blockFlags

getFlags :: Bits a => a -> [AccessFlag]
getFlags blockFlags = [ flag | (i, flag) <- flags, blockFlags `testBit` i ]
  where
    flags = [ ( 0, FPublic)
            , ( 1, FPrivate)
            , ( 2, FProtected)
            , ( 3, FStatic)
            , ( 4, FFinal)
            , ( 5, FSynchronized)
            , ( 6, FBridge)
            , ( 7, FVarargs)
            , ( 8, FNative)
              -- gap
            , (10, FAbstract)
            , (11, FStrict)
            , (12, FSynthetic)
            ]