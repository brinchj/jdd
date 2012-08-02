{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
  #-}

module Jimple.Typing where

import Jimple.Types


import Data.Either
import qualified Data.Foldable as F


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
  typeOf (E_length v) = typeOf v
  typeOf (E_cast t v) = Left t
  typeOf (E_instanceOf v _) = Left T_boolean
  typeOf (E_newArray t v) = Left $ T_array 1 t
  typeOf (E_new ref) = Right $ VarRef ref
  typeOf (E_newMultiArray t v _) = Left $ T_array undefined t

  -- comparisons
  typeOf e | isCmp e = Left T_boolean

  -- default: binary operators
  typeOf e = limitT $ map typeOf $ F.toList e


instance TypeableJ Value Value where
  typeOf (VConst c) = typeOf c
  typeOf (VLocal v) = Right v
  typeOf (VExpr  e) = typeOf e