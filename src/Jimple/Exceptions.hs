module Jimple.Exceptions where

import Prelude()
import CustomPrelude

import qualified Data.List as L
import qualified Data.Map  as M

import Jimple.Types (ExceptTable(..), ExceptEntry(..))


cleanupExcTable :: ExceptTable -> ExceptTable
cleanupExcTable (ExceptTable et0) = ExceptTable et1
  where
    -- Sort by first appearing then longest block (to - from)
    et1 = go $ L.sortBy (comparing eOrder) et0
    eOrder ExceptEntry{..} = (exceptFrom, negate $ exceptTarget - exceptFrom)

    -- Remove compiler-generated catch-handlers
    -- TODO: Merge exceptions with identical target?
    go [] = []
    go (e@ExceptEntry{exceptTarget=targ}:es) =
      e : go (filter ((targ/=).exceptTarget) es)


fromStart :: ExceptTable -> Integer -> [ExceptEntry]
fromStart (ExceptTable ees) start =
  M.elems $ M.fromList
  [ ((exceptFrom, negate exceptTo), e)
  | e@(ExceptEntry{..}) <- ees, exceptFrom == start ]

fromTarget :: ExceptTable -> Integer -> Maybe ExceptEntry
fromTarget (ExceptTable et) targ = listToMaybe $
                                   filter ((targ==).exceptTarget) et
