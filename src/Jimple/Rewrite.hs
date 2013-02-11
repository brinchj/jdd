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
       , mapRewritePrefix

       , rewrite
       , rewritePrefix

       , many

       -- Utility functions
       , scopeLess
       , mapScope

       , ScopeAction(..)
       , concatMapScope
       )
       where

import Control.Applicative hiding (many)

import Control.Monad
import Control.Monad.Error    (runErrorT, ErrorT(..))
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
-- >>> goto (Nothing, SGoto (Label 42))
-- True
goto (Nothing, SGoto{}) = True
goto                   _ = False
gotoP = snd <$> satisfy goto


-- | Test if a LabelStmt contains SIf
-- >>> if_ (Label 21, SIf undefined $ Label 42)
-- True
if_ (_, SIf{}) = True
if_           _ = False
ifP = satisfy if_

-- | Test if a LabelStmt contains STry
-- >>> try_ (Label 21, STry (0, 0) 0)
-- True
try_ (_, STry{}) = True
try_            _ = False
tryP = satisfy try_

-- | Test if a LabelStmt contains SCatch
-- >>> catch_ (Label 21, SCatch (0, 0) (ExceptEntry 0 0 0 0) Nothing)
-- True
catch_ (_, SCatch{}) = True
catch_              _ = False
catchP = satisfy catch_

-- | Test if a Stmt is a SLookupSwitch
-- >>> switchStmt $ SLookupSwitch "name" undefined []
-- True
switchStmt SLookupSwitch{} = True
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
    f (_, SGoto{}) = False
    f (_, SIf{}  ) = False
    f (Nothing,  _) = True
    f _             = False


jumpLabel (SGoto      lbl) = Just lbl
jumpLabel (SIf   cond lbl) = Just lbl
jumpLabel _ = Nothing


jumpP = satisfy $ isJust . jumpLabel . snd



-- | Rewrite Jimple method code according to rule
mapRewrite rule m = m { methodStmts = go $ methodStmts m }
  where
    go ops = fromMaybe ops $ rewrite rule ops

mapRewritePrefix rule m = m { methodStmts = go $ methodStmts m }
  where
    go ops = fromMaybe ops $ rewritePrefix rule ops


-- | Rewrite a list of LabelStmt
-- >>> rewrite (labelLess >> return []) [(Just 42, SNop), (Nothing, SNop)]
-- Just [(Just 42,nop)]
-- >>> rewrite (throwError "rule doesn't match") []
-- Nothing
rewrite :: Parser [Item] -> [Item] -> Maybe [Item]
rewrite p = go
  where
    go [] = Nothing
    go xs = case rewritePrefix p xs of
      Just xs' -> Just xs'
      Nothing  -> case xs of
        ((lbl, x):rest) | Just x1 <- goStmt x -> Just $ (lbl, x1) : rest
        (s:rest) -> (s:) `fmap` go rest
        _ -> Nothing

    goStmt s = case s of
      SIfElse cnd left right
        | Just [left1, right1] <- goAny [left, right] ->
          Just $ SIfElse cnd left1 right1

      SDoWhile name body cnd
        | Just body1 <- go body -> Just $ SDoWhile name body1 cnd

      SSwitch name v cs
        | Just result <- goAny $ map snd cs ->
          Just $ SSwitch name v $ zip (map fst cs) result

      STryCatch body cs fin
        | Just body1 <- go body -> Just $ STryCatch body1 cs fin

      STryCatch body cs fin
        | Just css <- goAny $ map snd cs ->
          Just $ STryCatch body (zip (map fst cs) css) fin

      STryCatch body cs (Just fin)
        | Just fin1 <- go fin -> Just $ STryCatch body cs $ Just fin1

      _ -> Nothing

    goAny [] = Nothing
    goAny (x:xs)
      | Just result <- go x = Just $ result : xs
      | otherwise           = (x:) <$> goAny xs


rewritePrefix :: Parser [Item] -> [Item] -> Maybe [Item]
rewritePrefix p = go
  where
    go [] = Nothing
    go xs = case runIdentity $ runErrorT $ runPT (play p) () "rewrite" xs of
      Right (Right (size, xs')) -> Just $ xs' ++ drop size xs
      _ -> Nothing

    play p = do
      start <- sourceLine <$> getPosition
      res <- p
      stop <- sourceLine <$> getPosition
      return (stop - start, res)


-- | Utility functions


scopeLess (SIfElse   cnd  _ _  ) = SIfElse   cnd  [] []
scopeLess (SDoWhile  name _ cnd) = SDoWhile  name [] cnd
scopeLess (SSwitch   name v _  ) = SSwitch   name v  []
scopeLess (STryCatch _    _ _  ) = STryCatch []   [] Nothing
scopeLess stmt                   = stmt



mapScope line scope scopeLine how =
  concatMapScope (liftM pure . line) scope scopeLine how

data ScopeAction v = Ignore
                   | AsLine
                   | Replace [LabelStmt v]
                   | Follow

concatMapScope :: (Functor m, Monad m) =>
                  (LabelStmt v -> m [LabelStmt v]) ->
                  (m [[LabelStmt v]] -> m [[LabelStmt v]]) ->
                  (LabelStmt v -> m (LabelStmt v)) ->
                  (LabelStmt v -> m (ScopeAction v)) ->
                  [LabelStmt v] -> m [LabelStmt v]
concatMapScope line scope scopeLine how stmts = goAll stmts
  where
    goAll ls = concat <$> mapM go ls

    go (pair@(lbl, lstmt)) = do
      let return1 = return . pure
      let scopeN' l f = scopeN l >>= \l' -> return1 $ f l'
      let ignore = return [pair]
      action <- how pair
      case action of
        Ignore      -> ignore
        AsLine      -> line pair
        Replace new -> return new
        Follow      -> do
          case lstmt of
            SIfElse cnd l r -> do
              (lbl', SIfElse cnd' _ _) <- scopeLine (lbl, SIfElse cnd [] [])
              scopeN' [l, r] $ \[l', r'] -> (lbl', SIfElse cnd' l' r')

            SDoWhile name body cnd -> do
              (lbl', SDoWhile name' _ cnd') <- scopeLine (lbl, SDoWhile name [] cnd)
              scopeN' [body] $ \[body'] -> (lbl', SDoWhile name' body' cnd')

            SSwitch name v cs -> do
              -- Create a single scope for the whole block
              (lbl', SSwitch name' v' _) <- scopeLine $ (lbl, SSwitch name v [])
              csSnd <- scope1 $ map snd cs
              return1 (lbl', SSwitch name' v' $ zip (map fst cs) csSnd)

            STryCatch body cs mfin -> do
              -- Scope each block separately
              (lbl', _) <- scopeLine (lbl, STryCatch [] [] Nothing)
              body':csSnd <- scopeN $ body:map snd cs
              mfin' <- maybe (return mfin) (liftM Just . single scopeN) mfin
              return1 $ (lbl, STryCatch body' (zip (map fst cs) csSnd) mfin')

            -- Cannot follow something with no scopes
            _ -> ignore


    -- Convert from ([[a]] -> m [[a]]) to ([a] -> m [a])
    single f = liftM head . f . liftM pure

    -- Create one scope for all the blocks (e.g. switch)
    scope1 = scope . mapM goAll

    -- Create a seperate scope for each block (e.g. if)
    scopeN = mapM $ single scope . goAll
