{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
  #-}

module Jimple.Rewrite
       ( Parser
       , Item
       , satisfy
       , goto
       , gotoP
       , label
       , labelP
       , jumpless
       , rewrite
       , many )
       where

import Control.Applicative hiding (many)
import Control.Monad

import Data.Maybe
import Text.Parsec hiding (satisfy, label)

import Jimple.Types


type Item   = (Maybe Label, Stmt Value)
type Parser = Parsec [Item] ()


satisfyWith :: (Item -> Maybe Item) -> Parser Item
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


rewrite :: Parser (Int, [Item]) -> [Item] -> Maybe [Item]
rewrite p xs = go xs
  where
    go [] = Nothing
    go xs = case runP p () "" xs of
      Left _            -> (head xs:) `fmap` go (tail xs)
      Right (size, xs') -> Just $ xs' ++ drop size xs

