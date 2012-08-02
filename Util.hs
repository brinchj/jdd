{-# LANGUAGE FlexibleContexts #-}

module Util where

import qualified Control.Monad.State as ST


modifyFst :: ST.MonadState (a, b) m => (a -> a) -> m ()
modifyFst f = ST.modify $ \(a, b) -> (f a, b)

modifySnd :: ST.MonadState (a, b) m => (b -> b) -> m ()
modifySnd f = ST.modify $ \(a, b) -> (a, f b)

