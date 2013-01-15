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
       , try_
       , catch_
       , switch_

       , ifP
       , tryP
       , catchP
       , switchP

       , label
       , labelLess
       , labelP

       , jumpless
       , jumpLabel
       , jumpP

       , switchStmt

       , rewrite
       , many )
       where

import Control.Applicative hiding (many)

import Control.Monad
import Control.Monad.Error    (runErrorT, ErrorT(..))
import Control.Monad.Identity (runIdentity, Identity)

import Data.Maybe
import Text.Parsec hiding (satisfy, label)

import Jimple.Types


type Item   = (Maybe Label, Stmt Value)
type Parser = ParsecT [Item] () (ErrorT String Identity)


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
ifP = satisfy if_

try_ (_, S_try _ _) = True
try_              _ = False
tryP = satisfy try_

catch_ (_, S_catch _ _ _) = True
catch_                  _ = False
catchP = satisfy catch_


switchStmt S_lookupSwitch{} = True
switchStmt _ = False

switch_ = switchStmt . snd
switchP = satisfy switch_


label     = satisfy hasLabel
labelLess = satisfy $ not . hasLabel

hasLabel (Just lbl, _) = True
hasLabel _             = False

labelP = fst <$> label


-- jumpless :: Parser Item
jumpless = satisfy f
  where
    f (_, S_goto _) = False
    f (_, S_if _ _) = False
    f (Nothing,  _) = True
    f _             = False


jumpLabel (S_goto      lbl) = Just lbl
jumpLabel (S_if   cond lbl) = Just lbl
jumpLabel _ = Nothing


jumpP = satisfy $ isJust . jumpLabel . snd


rewrite :: Parser [Item] -> [Item] -> Maybe [Item]
rewrite p xs = go xs
  where
    go [] = Nothing
    go xs = case runIdentity $ runErrorT $ runPT (play p) () "rewrite" xs of
      Right (Right (size, xs')) -> Just $ xs' ++ drop size xs
      _ ->
        case xs of
          ((lbl, S_ifElse cnd left right):rest)
            | Just [left, right] <- goAny [left, right] ->
              Just $ (lbl, S_ifElse cnd left right) : rest

          ((lbl, S_doWhile name body cnd):rest) | isJust $ go body ->
            Just $ (lbl, S_doWhile name (fromJust $ go body) cnd) : rest

          ((lbl, S_switch name v cs):rest)
            | Just result <- goAny $ map snd cs ->
              Just $ (lbl, S_switch name v $ zip (map fst cs) result) : rest

          _ -> (head xs:) `fmap` go (tail xs)

    goAny [] = Nothing
    goAny (x:xs)
      | Just result <- go x = Just $ result : xs
      | otherwise           = (x:) <$> goAny xs

    play p = do
      start <- sourceLine <$> getPosition
      res <- p
      stop <- sourceLine <$> getPosition
      return (stop - start, res)