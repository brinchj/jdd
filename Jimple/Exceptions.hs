module Jimple.Exceptions where

import Data.Ord

import qualified Data.List as L
import qualified Data.Map  as M

import Jimple.Types (ExceptTable(..), ExceptEntry(..))


cleanupExcTable :: ExceptTable -> ExceptTable
cleanupExcTable (ExceptTable et0) = ExceptTable et1
  where
    -- Sort by first appearing then longest block (to - from)
    et1 = go $ L.sortBy (comparing eOrder) et0
    eOrder (ExceptEntry from to targ id) = (from, negate (to - from))

    -- Remove compiler-generated catch-handlers
    go (e:es) = e : case e of
      ExceptEntry _ _ targ 0 -> filter ((targ/=).exceptTarget) es
      _                      -> go es
