module Jimple.Typing where

import Prelude()
import CustomPrelude

import Jimple.Types

import qualified Jimple as J
import qualified Parser as CF

import qualified Data.Foldable as F
import qualified Data.Map as M

import qualified Control.Monad.State as ST

type TypeV v = Either Type (Variable v, Type -> Type)

class TypeableJ a v where
  typeOf :: a -> TypeV v


limitT :: [TypeV v] -> TypeV v
limitT ts = head $ map Left (lefts ts) ++ ts


instance TypeableJ Type v where
  typeOf = Left


instance TypeableJ Constant v where
  typeOf c = Left $ case c of
    CBoolean _ -> TBoolean
    CInt     _ -> TInt
    CDouble  _ -> TDouble
    CFloat   _ -> TFloat
    CLong    _ -> TLong
    CString  _ -> TObject "java/lang/String"
    CNull      -> TUnknown



instance TypeableJ (Variable v) v where
  typeOf v = Right (v, id)


isCmp :: Expression v -> Bool
isCmp e = case e of
   EEq   _ _ -> True
   EGe   _ _ -> True
   ELe   _ _ -> True
   ELt   _ _ -> True
   ENe   _ _ -> True
   EGt   _ _ -> True
   ECmp  _ _ -> True
   ECmpg _ _ -> True
   ECmpl _ _ -> True
   _          -> False

instance (TypeableJ v v) => TypeableJ (Expression v) v where
  typeOf (ELength _) = Left TInt
  typeOf (ECast t _) = Left t
  typeOf (EInstanceOf _ _) = Left TBoolean
  typeOf (ENewArray t _) = Left $ TArray 1 t
  typeOf (ENew (RObject c) _args) = Left $ TObject $ fromUtf8 $ CF.classPath c
  typeOf (ENewMultiArray t _v _) = Left $ TArray 0 t
  typeOf (EInvoke _ sig _) = Left $ methodResult sig

  -- comparisons
  typeOf e | isCmp e = Left TBoolean

  -- default: binary operators
  typeOf e = limitT $ map typeOf $ F.toList e


instance TypeableJ (Ref Value) Value where
  typeOf RThis = Left $ TObject "<this>"
  typeOf (RInstanceField _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf (RStaticField   _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf (RArray (VLocal v) _) = Right (v, \(TArray _ t) -> t)
  typeOf (RArray (VExpr  e) _) = typeOf e

  typeOf s = error $ "TypeableJ ref: " ++ show s

instance TypeableJ Value Value where
  typeOf (VConst c) = typeOf c
  typeOf (VLocal v) = Right (v, id)
  typeOf (VExpr  e) = typeOf e


type SimpleTyperST = (M.Map Local Type, M.Map Local Local)
simpleTyper :: JimpleMethod Value -> JimpleMethod Value
simpleTyper (meth@(Method sig ls _is ms _me)) =
  meth { methodLocalDecls = []
       , methodStmts = ms2
       }
  where
    -- State is types map and renaming map
    ms2 = ST.evalState (mapM go ms) (types, M.empty)
    types = M.fromList $ map (\(LocalDecl t l) -> (l, t)) ls

    -- Run an action while preserving the state
    isolate mv = ST.evalState mv <$> ST.get

    go :: LabelStmt Value -> ST.State SimpleTyperST (LabelStmt Value)
    go (lbl, s1) = do
      let go' = isolate <$> mapM go
      let def = return s1
      s2 <- case s1 of
        SAssign (VarLocal v) e -> do
          mt0 <- get v
          flip (flip maybe $ const def) mt0 $ do
            x <- isJust <$> ST.gets (M.lookup v . snd)
            set v $ typeOf' e
            mt1 <- get v
            return $ flip (maybe s1) mt1 $ \t1 ->
              if x then s1 else
                SDeclare t1 (VarLocal v) e

        SIfElse c left right -> liftM2 (SIfElse c) (go' left) (go' right)
        SSwitch n e sls      -> liftM (SSwitch n e) $ handleSwitch sls
        SDoWhile n body v    -> liftM (flip (SDoWhile n) v) (go' body)
        STryCatch body cs0 mfn -> do
          let fnM = case mfn of
                Just fn -> Just <$> go' fn
                Nothing -> return Nothing
          liftM3 STryCatch (go' body) (handleExcepts cs0) fnM

        _ -> def

      m <- ST.gets snd
      return (lbl, rename m `fmap` s2)

    handleSwitch cs = forM cs $ \(c, body) ->
      (c,) <$> mapM go body

    handleExcepts cs = forM cs $ \(exc, body) -> isolate $ do
      let go' = mapM go
      set (Local "exc") $ Left $ TObject $ fromUtf8 $ CF.classPath exc
      (exc,) <$> go' body


    rename m (VLocal (VarLocal l)) = VLocal $ VarLocal $
                                     fromMaybe l $ M.lookup l m
    rename _ v = v

    get l = M.lookup l <$> ST.gets fst

    set :: Local -> TypeV Value -> ST.State SimpleTyperST ()
    set (Local "_") _ = return ()

    set (Local nm) (Left t) = do
      let nms = nm:[nm ++ "_" ++ showT i | i <- [2..]]
      nm2 <- Local <$> findMatch t nms
      modifySnd $ M.insert (Local nm) nm2

    set (Local nm) (Right (VarLocal l, conv)) = do
      mt <- ST.gets $ M.lookup l . fst
      case mt of
        Nothing -> return ()
        Just t  -> set (Local nm) $ Left $ conv t

    set (Local nm) (Right (VarRef r, conv)) =
      set (Local nm) $ typeOf' r

    set a b = let err c = error $
                          "not prepared for: " ++ show a ++ ", " ++ show c
              in case b of
                Left c       -> err c
                Right (c, _) -> err c


    findMatch t (nm:nms) = do
      mt <- ST.gets $ M.lookup (Local nm) . fst
      case mt of
        Nothing -> do modifyFst $ M.insert (Local nm) t
                      return nm
        Just t2 | t == t2   -> return nm
                | otherwise -> findMatch t nms

    typeOf' t = case typeOf t of
      Left (TObject "<this>") ->
        Left $ TObject $ fromUtf8 $ CF.classPath $ methodClass sig
      t1 -> t1