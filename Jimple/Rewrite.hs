{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
  #-}

module Jimple.Rewrite
       ( Parser
       , Item
       , satisfy
       , anyStmt

       , goto
       , gotoP
       , if_

       , label
       , labelP

       , jumpless
       , jumpLabel
       , jumpP

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

anyStmt = satisfy $ const True


goto (Nothing, S_goto _) = True
goto                   _ = False
gotoP = snd <$> satisfy goto


if_ (_, S_if _ _) = True
if_             _ = False


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


jumpLabel (S_goto      lbl) = Just lbl
jumpLabel (S_if   cond lbl) = Just lbl
jumpLabel _ = Nothing


jumpP = satisfy $ maybe False (const True) . jumpLabel . snd


rewrite :: Parser (Int, [Item]) -> [Item] -> Maybe [Item]
rewrite p xs = go xs
  where
    go [] = Nothing
    go xs = case runP p () "" xs of
      Left _ ->
        case xs of
          ((lbl, S_ifElse cnd left right):rest) | isJust $ go left ->
            Just $ (lbl, S_ifElse cnd (fromJust $ go left) right) : rest

          ((lbl, S_ifElse cnd left right):rest) | isJust $ go right ->
            Just $ (lbl, S_ifElse cnd left (fromJust $ go right)) : rest

          ((lbl, S_doWhile name body cnd):rest) | isJust $ go body ->
            Just $ (lbl, S_doWhile name (fromJust $ go body) cnd) : rest

          _ -> (head xs:) `fmap` go (tail xs)
      Right (size, xs') -> Just $ xs' ++ drop size xs

