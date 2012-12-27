{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
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

type TypeV v = Either Type (Variable v)

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
  typeOf = Right



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
  typeOf (E_new (R_object c)) = Left $ T_object $ CF.classPath c
  typeOf (E_newMultiArray t v _) = Left $ T_array undefined t
  typeOf (E_invoke _ sig _) = Left $ methodResult sig

  -- comparisons
  typeOf e | isCmp e = Left T_boolean

  -- default: binary operators
  typeOf e = limitT $ map typeOf $ F.toList e


instance Show v => TypeableJ (Ref v) v where
  typeOf R_this = Left $ T_object "this"
  typeOf (R_instanceField _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf (R_staticField   _ desc) = Left $ J.typeFromBS' $ CF.descType desc
  typeOf s = error $ show s

instance TypeableJ Value Value where
  typeOf (VConst c) = typeOf c
  typeOf (VLocal v) = Right v
  typeOf (VExpr  e) = typeOf e


type SimpleTyperST = (M.Map Local Type, M.Map Local Local)
simpleTyper :: JimpleMethod Value -> JimpleMethod Value
simpleTyper (meth@(Method _ ls is ms me)) =
  meth { methodLocalDecls = ls2
       , methodStmts = zip (map fst ms) ms2
       }
  where
    ls2 = map (uncurry $ flip LocalDecl) $ M.toList types2

    rMap = M.empty

    (ms2, (types2, _)) = ST.runState (mapM go $ map snd ms) (types, rMap)
    types = M.fromList $ map (\(LocalDecl t l) -> (l, t)) ls

    db a = traceShow a a

    go :: Stmt Value -> ST.State SimpleTyperST (Stmt Value)
    go s = do case s of
                S_assign (VarLocal v) e -> set v $ typeOf e
                _ -> return ()
              m <- ST.gets snd
              return $ rename m `fmap` s

    rename m (VLocal (VarLocal l)) = VLocal $ VarLocal $
                                     fromMaybe l $ M.lookup l m
    rename _ v = v

    set :: Local -> TypeV Value -> ST.State SimpleTyperST ()
    set (Local "_") _ = return ()

    set (Local nm) (Left t) = do
      let nms = nm:[nm ++ '_' : show i | i <- [2..]]
      nm2 <- Local <$> findMatch t nms
      modifySnd $ M.insert (Local nm) nm2

    set (Local nm) (Right (VarLocal l)) = do
      mt <- ST.gets $ M.lookup l . fst
      case mt of
        Nothing -> return ()
        Just t  -> set (Local nm) $ Left t

    set (Local nm) (Right (VarRef r)) = do
      set (Local nm) $ typeOf r

    set a b = error $ "not prepared for: " ++ show a ++ ", " ++ show b


    findMatch t (nm:nms) = do
      mt <- ST.gets $ M.lookup (Local nm) . fst
      case mt of
        Nothing -> do modifyFst $ M.insert (Local nm) t
                      return nm
        Just t2 | t == t2   -> return nm
                | otherwise -> findMatch t nms

