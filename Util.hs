{-# LANGUAGE FlexibleContexts #-}

module Util where

import qualified Control.Monad.State as ST
import Control.Arrow


modifyFst :: ST.MonadState (a, b) m => (a -> a) -> m ()
modifyFst f = ST.modify $ first f

modifySnd :: ST.MonadState (a, b) m => (b -> b) -> m ()
modifySnd f = ST.modify $ second f

