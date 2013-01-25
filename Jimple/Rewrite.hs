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

       , mapRewrite
       , rewrite
       , many )
       where

import Control.Applicative hiding (many)

import Control.Monad
import Control.Monad.Error    (runErrorT, ErrorT(..), throwError)
import Control.Monad.Identity (runIdentity, Identity)

import Data.Maybe
import Text.Parsec hiding (satisfy, label)

import Jimple.Types


type Item   = LabelStmt Value
type Parser = ParsecT [Item] () (ErrorT String Identity)


satisfyWith :: (Item -> Maybe Item) -> Parser Item
satisfyWith f = tokenPrim showT nextPos testT
    where
      showT           = show
      testT           = f
      nextPos pos _ _ = incSourceLine pos 1


-- | Parse a statement iff it satisfies the predicate function 'f'
satisfy :: (Item -> Bool) -> Parser Item
satisfy f = satisfyWith $ \x -> guard (f x) >> return x

-- | Parse the next statement
anyStmt = satisfy $ const True


-- | Test if a LabelStmt is a goto
-- >>> goto (Nothing, S_goto (Label 42))
-- True
goto (Nothing, S_goto{}) = True
goto                   _ = False
gotoP = snd <$> satisfy goto


-- | Test if a LabelStmt contains S_if
-- >>> if_ (Label 21, S_if undefined $ Label 42)
-- True
if_ (_, S_if{}) = True
if_           _ = False
ifP = satisfy if_

-- | Test if a LabelStmt contains S_try
-- >>> try_ (Label 21, S_try (0, 0) 0)
-- True
try_ (_, S_try{}) = True
try_            _ = False
tryP = satisfy try_

-- | Test if a LabelStmt contains S_catch
-- >>> catch_ (Label 21, S_catch (0, 0) (ExceptEntry 0 0 0 0) Nothing)
-- True
catch_ (_, S_catch{}) = True
catch_              _ = False
catchP = satisfy catch_

-- | Test if a Stmt is a S_lookupSwitch
-- >>> switchStmt $ S_lookupSwitch "name" undefined []
-- True
switchStmt S_lookupSwitch{} = True
switchStmt _ = False

switch_ = switchStmt . snd
switchP = satisfy switch_


label     = satisfy hasLabel
labelLess = satisfy $ not . hasLabel

hasLabel (Just lbl, _) = True
hasLabel _             = False

labelP = fst <$> label


jumpless = satisfy f
  where
    f (_, S_goto{}) = False
    f (_, S_if{}  ) = False
    f (Nothing,  _) = True
    f _             = False


jumpLabel (S_goto      lbl) = Just lbl
jumpLabel (S_if   cond lbl) = Just lbl
jumpLabel _ = Nothing


jumpP = satisfy $ isJust . jumpLabel . snd



-- | Rewrite Jimple method code according to rule
mapRewrite rule m = m { methodStmts = go $ methodStmts m }
  where
    go ops = maybe ops go $ rewrite rule ops


-- | Rewrite a list of LabelStmt
-- >>> rewrite (labelLess >> return []) [(Just 42, S_nop), (Nothing, S_nop)]
-- Just [(Just 42,nop)]
-- >>> rewrite (throwError "rule doesn't match") []
-- Nothing
rewrite :: Parser [Item] -> [Item] -> Maybe [Item]
rewrite p = go
  where
    go [] = Nothing
    go xs = case runIdentity $ runErrorT $ runPT (play p) () "rewrite" xs of
      Right (Right (size, xs')) -> Just $ xs' ++ drop size xs
      _ -> case xs of
        ((lbl, x):rest) | Just x1 <- goStmt x -> Just $ (lbl, x1) : rest
        _ -> (head xs:) `fmap` go (tail xs)

    goStmt s = case s of
      S_ifElse cnd left right
        | Just [left1, right1] <- goAny [left, right] ->
          Just $ S_ifElse cnd left1 right1

      S_doWhile name body cnd
        | Just body1 <- go body -> Just $ S_doWhile name body1 cnd

      S_switch name v cs
        | Just result <- goAny $ map snd cs ->
          Just $ S_switch name v $ zip (map fst cs) result

      S_tryCatch body cs fin
        | Just body1 <- go body -> Just $ S_tryCatch body1 cs fin

      S_tryCatch body cs fin
        | Just css <- goAny $ map snd cs ->
          Just $ S_tryCatch body (zip (map fst cs) css) fin

      S_tryCatch body cs (Just fin)
        | Just fin1 <- go fin -> Just $ S_tryCatch body cs $ Just fin1

      _ -> Nothing

    goAny [] = Nothing
    goAny (x:xs)
      | Just result <- go x = Just $ result : xs
      | otherwise           = (x:) <$> goAny xs

    play p = do
      start <- sourceLine <$> getPosition
      res <- p
      stop <- sourceLine <$> getPosition
      return (stop - start, res)