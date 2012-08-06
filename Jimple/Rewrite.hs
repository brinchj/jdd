{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
  #-}

module Rewrite where

import Control.Monad

import Data.Maybe
import Text.Parsec hiding (satisfy, label)

import Jimple.Types


type Item   = (Maybe Label, Stmt Value)
type Parser = Parsec [Item] ()


satisfy f = tokenPrim showT nextPos testT
    where
      showT           = show
      testT x         = if f x then Just x else Nothing
      nextPos pos _ _ = incSourceLine pos 1


goto = satisfy f
  where
    f (Nothing, S_goto _) = True
    f                   _ = False

label = satisfy f
  where
    f (Just lbl, _) = True
    f             _ = False


jumpless :: Parser Item
jumpless = satisfy f
  where
    f (Nothing, S_goto _) = False
    f (Nothing, S_if _ _) = False
    f _                   = True


-- Eliminate cross goto'ing (..0 -> 2..2 -> 1..1 -> 3.. => 0..1..2..3)
elimGoto :: Parser (Int, [Item])
elimGoto = do
  (Nothing, S_goto lbl2)  <- goto
  body1Top@(Just lbl1, _) <- label

  body1 <- many jumpless

  (Nothing, S_goto lbl3)   <- goto
  body2Top@(Just lbl2', _) <- label
  guard $ lbl2 == lbl2'

  body2 <- many jumpless

  (Nothing, S_goto lbl1')  <- goto
  guard $ lbl1 == lbl1'

  body3Top@(Just lbl3', _) <- label
  guard $ lbl3 == lbl3'

  let size = 6 + (length $ concat [body1, body2])
  let body = (body2Top:body2) ++ (body1Top:body1) ++ [body3Top]

  return (size, body)


rewrite :: Parser (Int, [Item]) -> [Item] -> Maybe [Item]
rewrite p xs = go xs
  where
    go [] = Nothing
    go xs = case runP p () "" xs of
      Left _            -> (head xs:) `fmap` (go $ tail xs)
      Right (size, xs') -> Just $ xs' ++ drop size xs

