module Jimple.Exceptions where

import Data.Maybe
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
    go []     = []
    go (e:es) = e : case e of
      ExceptEntry _ _ targ 0 -> go $ filter ((targ/=).exceptTarget) es
      _                      -> go es


fromStart :: ExceptTable -> Integer -> [ExceptEntry]
fromStart (ExceptTable et) start = M.elems $ M.fromList [
  ((exceptFrom e, -1 * exceptTo e), e) | e <- et, exceptFrom e == start ]

fromTarget :: ExceptTable -> Integer -> Maybe ExceptEntry
fromTarget (ExceptTable et) targ = listToMaybe $
                                   filter ((targ==).exceptTarget) et
