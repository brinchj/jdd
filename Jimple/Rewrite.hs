{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
  #-}

module Rewrite where

import Control.Applicative hiding (many)
import Control.Monad

import Data.Maybe
import Text.Parsec hiding (satisfy, label)

import Jimple.Types


type Item   = (Maybe Label, Stmt Value)
type Parser = Parsec [Item] ()


satisfyWith f = tokenPrim showT nextPos testT
    where
      showT           = show
      testT           = f
      nextPos pos _ _ = incSourceLine pos 1


satisfy f = satisfyWith $ \x -> if f x then Just x else Nothing


goto = satisfy f
  where
    f (Nothing, S_goto _) = True
    f                   _ = False
gotoP = snd <$> goto


label = satisfy f
  where
    f (Just lbl, _) = True
    f             _ = False
labelP = fst <$> label


jumpless :: Parser Item
jumpless = satisfy f
  where
    f (_, S_goto _) = False
    f (_, S_if _ _) = False
    f (Nothing,  _) = True
    f _             = False


-- Eliminate cross goto:
-- goto 2
-- 1: ...
-- goto 3:
-- 2: ...
-- goto 1
-- 3:
-- ==>
-- 1: ...
-- 2: ...
-- 3:
elimGoto :: Parser (Int, [Item])
elimGoto = do
  S_goto lbl2 <- gotoP
  body1Top@(Just lbl1, _) <- label

  body1 <- many jumpless

  S_goto lbl3 <- gotoP
  body2Top@(Just lbl2', _) <- label
  guard $ lbl2 == lbl2'

  body2 <- many jumpless

  S_goto lbl1' <- gotoP
  guard $ lbl1 == lbl1'

  body3Top@(Just lbl3', _) <- label
  guard $ lbl3 == lbl3'

  -- compute line size of parsed statements
  let size = 6 + length (body1 ++ body2)
  -- rearrange blocks without gotos
  let body = (body2Top:body2) ++ (body1Top:body1) ++ [body3Top]

  return (size, body)


rewrite :: Parser (Int, [Item]) -> [Item] -> Maybe [Item]
rewrite p xs = go xs
  where
    go [] = Nothing
    go xs = case runP p () "" xs of
      Left _            -> (head xs:) `fmap` go (tail xs)
      Right (size, xs') -> Just $ xs' ++ drop size xs

