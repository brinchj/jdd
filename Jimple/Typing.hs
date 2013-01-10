{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TupleSections
  #-}

module Jimple.Typing where

import Util
import Jimple.Types

import qualified Jimple as J
import qualified Parser as CF

import Data.Either
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as ST

import Debug.Trace

type TypeV v = Either Type (Variable v, Type -> Type)

class TypeableJ a v where
  typeOf :: a -> TypeV v


limitT :: [TypeV v] -> TypeV v
limitT ts = head $ map Left (lefts ts) ++ ts


instance TypeableJ Type v where
  typeOf = Left


instance TypeableJ Constant v where
  typeOf c = Left $ case c of
    C_int    _ -> T_int
    C_double _ -> T_double
    C_float  _ -> T_float
    C_long   _ -> T_long
    C_string _ -> T_object "java/lang/String"
    C_null     -> T_unknown



instance TypeableJ (Variable v) v where
  typeOf v = Right (v, id)



isCmp e = case e of
   E_eq   _ _ -> True
   E_ge   _ _ -> True
   E_le   _ _ -> True
   E_lt   _ _ -> True
   E_ne   _ _ -> True
   E_gt   _ _ -> True
   E_cmp  _ _ -> True
   E_cmpg _ _ -> True
   E_cmpl _ _ -> True
   _          -> False

instance (TypeableJ v v) => TypeableJ (Expression v) v where
  typeOf (E_length v) = Left T_int
  typeOf (E_cast t v) = Left t
  typeOf (E_instanceOf v _) = Left T_boolean
  typeOf (E_newArray t v) = Left $ T_array 1 t
  typeOf (E_new (R_object c) _args) = Left $ T_object $ CF.classPath c
  typeOf (E_newMultiArray t v _) = Left $ T_array 0 t
  typeOf (E_invoke _ sig _) = Left $ methodResult sig

  -- comparisons
  typeOf e | isCmp e = Left T_boolean

  -- default: binary operators
  typeOf e = limitT $ map typeOf $ F.toList e


instance TypeableJ (Ref Value) Value where
  typeOf R_this = Left $ T_object "<this>"
  typeOf (R_instanceField _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf (R_staticField   _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf (R_array (VLocal v) _) = Right (v, \(T_array _ t) -> t)
  typeOf (R_array (VExpr  e) _) = typeOf e

  typeOf s = error $ "TypeableJ ref: " ++ show s

instance TypeableJ Value Value where
  typeOf (VConst c) = typeOf c
  typeOf (VLocal v) = Right (v, id)
  typeOf (VExpr  e) = typeOf e


type SimpleTyperST = (M.Map Local Type, M.Map Local Local)
simpleTyper :: JimpleMethod Value -> JimpleMethod Value
simpleTyper (meth@(Method sig ls is ms me)) =
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
      let go' = mapM (isolate . go)
      let def = return s1
      s2 <- case s1 of
        S_assign (VarLocal v) e -> do
          mt0 <- get v
          flip (flip maybe $ const def) mt0 $ do
              set v $ typeOf' e
              mt1 <- get v
              return $ flip (maybe s1) mt1 $ \t1 -> S_declare t1 (VarLocal v) e

        S_ifElse c left right -> liftM2 (S_ifElse c) (go' left) (go' right)
        S_switch n e ls       -> liftM (S_switch n e) $ handleSwitch ls
        S_doWhile n body v    -> liftM (flip (S_doWhile n) v) (go' body)
        S_tryCatch body cs0   -> liftM2 S_tryCatch (go' body) $ handleExcepts cs0

        _ -> def

      m <- ST.gets snd
      return (lbl, rename m `fmap` s2)

    handleSwitch cs = forM cs $ \(c, body) ->
      (c,) <$> mapM go body

    handleExcepts cs = forM cs $ \c -> isolate $ do
      let go' = mapM go
      case c of
        (Just exc, body) -> do
          set (Local "exc") $ Left $ T_object $ CF.classPath exc
          (Just exc,) <$> go' body
        (Nothing, body) ->
          (Nothing,) <$> go' body


    rename m (VLocal (VarLocal l)) = VLocal $ VarLocal $
                                     fromMaybe l $ M.lookup l m
    rename _ v = v

    get l = M.lookup l <$> ST.gets fst

    set :: Local -> TypeV Value -> ST.State SimpleTyperST ()
    set (Local "_") _ = return ()

    set (Local nm) (Left t) = do
      let nms = nm:[nm ++ '_' : show i | i <- [2..]]
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
      Left (T_object "<this>") ->
        Left $ T_object $ CF.classPath $ methodClass sig
      t1 -> t1