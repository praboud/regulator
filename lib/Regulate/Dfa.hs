module Regulate.Dfa
    ( compileEnfaToDfa
    , compileEnfaToDfaExtra
    , tokenize
    , accept
    ) where


import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array (Array, (!), array, bounds)
import Data.Ix (range, inRange)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, mapMaybe)
import Data.Char (chr)

import Control.Monad (foldM, (>=>))
import Control.Arrow (first, second)

import Regulate.Enfa (epsilonClosure)
import Regulate.Types
import Regulate.Util (sortAndGroupOn)

-- intemediate representation of a DFA transitions, used during DFA construction
type DFAMap = Map (Set State) (Map Symbol (Set State))

{- is a string accepted by the regex defined by the dfa?
 - if so, return Just the state that the DFA terminates in
 - otherwise, return nothing
 -}
accept :: DFA -> [Symbol] -> Maybe State
accept (DFA ts q0 as) = foldM transition q0 >=> (\q -> if Set.member q as then Just q else Nothing)
    where
    transition q c = if inRange (bounds ts) (q, c) then ts ! (q, c) else Nothing

{- break the entire input string into tokens which are in the language of
 - the provided DFA, or return an error message if the string does not fit
 - that language
 -}
tokenize :: DFA -> [Symbol] -> Either String [Token Int]
tokenize (DFA ts q0 as) = tok_h [] q0
    where
    err rs q
        | Set.member q as = Right $ Token q rs'
        | otherwise = Left $ "Error, read '" ++ rs' ++ "'"
        where
        rs' = reverse rs
    tok_h rs q us
        | null us = fmap (:[]) $ err rs q
        | isNothing q' = do
            tok <- err rs q
            toks <- tok_h [] q0 us
            return (tok:toks)
        | otherwise = tok_h (u:rs) (fromJust q') (tail us)
        where
        u = head us
        q' = ts ! (q, u)

compileEnfaToDfa :: ENFA -> DFA
compileEnfaToDfa enfa@(ENFA _ _ as) = fst $ compileEnfaToDfaExtra (Set.null . Set.intersection as) enfa


-- for clarity
type ENFAState = State
type DFAState = State
{-
 - take an epsilon-NFA and turn it into the equivalent DFA, as well as
 - returning some diagnostic information - a map of DFA states to sets
 - of ENFA states (this helps work out what the final states actually mean,
 - and what final state indicates what token)
 -
 - this function is rather complicated, and you should probably know the ins
 - and outs of ENFAs and DFAs before looking at this
 -}
compileEnfaToDfaExtra :: Ord x => (Set State -> x) -> ENFA -> (DFA, Map Int (Set State))
compileEnfaToDfaExtra mergeKey enfa@(ENFA ts q0 as) = (DFA transitionArray dfaStart acceptStates, codeToState)
    where
    dfaStart = (stateToCode Map.!) $ head $ filter (Set.member q0) states

    -- get a list of all symbols in use
    syms = concatMap (mapMaybe id . Map.keys) $ Map.elems ts

    -- get bounds on those symbols
    (minSym, maxSym) = case syms of
        [] -> (chr 0, chr 0)
        _ -> (minimum syms, maximum syms)

    -- build up the actual 2D array which defines, given the current state
    -- and symbol seen by the matching process, which symbol to go to next
    -- take each possible symbol and DFA state, and
    -- 1) find the equivalent set of ENFA states
    -- 2) find the set of ENFA states that would be transitioned to
    -- 3) find the DFA state equivalent to that set of ENFA states
    transitionArray :: Array (DFAState, Symbol) (Maybe DFAState)
    transitionArray = array arrayBounds
                      $ map (\p@(s, c) -> (p, Map.lookup s codeToState
                                          >>= flip Map.lookup transitions
                                          >>= Map.lookup c
                                          >>= flip Map.lookup stateToCode))
                      $ range arrayBounds
    arrayBounds = ((0, minSym), (length states - 1, maxSym))

    -- for converting between ENFA state sets and DFA states
    stateToCode :: Map (Set ENFAState) DFAState
    stateToCode = foldr (\(qs, i) m -> Map.insert qs i m) Map.empty $ zip states [0..]
    codeToState :: Map DFAState (Set ENFAState)
    codeToState = foldr (\(qs, i) m -> Map.insert i qs m) Map.empty $ zip states [0..]

    transitions = compressDFAMap mergeKey $ buildTransitions Map.empty $ epsilonClosure enfa $ Set.singleton q0
    -- every state used in the transitions "proto-dfa"
    states = Set.toList $ Map.foldr (flip $ Map.foldr Set.insert) (Map.keysSet transitions) transitions

    acceptStates :: Set DFAState
    -- take accept states and add all states that can reach an accept state
    -- via an epsilon transition -- (ie: include the accept state in their epsilon closure)
    acceptStates = Map.foldrWithKey (\qs c ac -> (if overlap as qs then Set.insert c else id) ac) Set.empty stateToCode

    overlap :: Ord x => Set x -> Set x -> Bool
    overlap x y = not $ Set.null $ Set.intersection x y

    -- build up a graph which is essentially a dfa whose states are sets of
    -- ENFA states. We consider starting at the start state of the enfa
    -- and following all possible routes, building up a dfa as we go.
    -- If there are multiple transitions from the same state on the same symbol,
    -- then we take all of them, and essentially are in all possible states
    -- simultaneously
    buildTransitions :: DFAMap -> Set State -> DFAMap
    buildTransitions ts' qs
        | Map.member qs ts' = ts' -- we have already encountered that state, do nothing
        | otherwise = Map.foldr (flip buildTransitions) (Map.insert qs neighbours ts') neighbours
        where
        nonEmptyTransitions :: State -> Map Symbol (Set State)
        nonEmptyTransitions = maybe Map.empty (Map.mapKeysMonotonic fromJust . Map.filterWithKey (\k _ -> isJust k)) . flip Map.lookup ts

        -- all transitions available from qs' (see above)
        neighbours :: Map Symbol (Set State)
        neighbours = Map.map (epsilonClosure enfa)
                     $ Set.foldr (\q m -> Map.unionWith Set.union m $ nonEmptyTransitions q) Map.empty qs

-- remove duplicate states: those that have exactly the same outbound transitions
-- TODO: for lexers, we must consider if the states translate into accepting the same token
type ENFAList = [(Set State, Map Symbol (Set State))]
compressDFAMap :: Ord x => (Set State -> x) -> DFAMap -> DFAMap
compressDFAMap mergeKey = Map.fromList . reduce . Map.toList
    where
    reduce :: ENFAList -> ENFAList
    reduce dfaml
        | Map.null mapping = dfaml
        | otherwise = reduce $ map (second (Map.map remap)) reduced
        where
        -- get a reduced list of states, as well as a mapping of the states
        -- removed to their equivalent states
        -- remove all states that have another state with the same output transitions,
        -- and that are both accept states
        (reduced, mapping) = foldr processDuplicates ([], Map.empty)
                             $ sortAndGroupOn (first mergeKey) dfaml
        remap q = fromMaybe q $ Map.lookup q mapping

    processDuplicates :: ENFAList -> (ENFAList, Map (Set State) (Set State)) -> (ENFAList, Map (Set State) (Set State))
    processDuplicates [] _ = undefined
    processDuplicates xs@((_, ts):_) (ys, mapping) = ((common, ts):ys, mapping')
        where
        mapping' = case xs of
            [] -> undefined
            [_] -> mapping
            _ -> foldr (flip Map.insert common . fst) mapping xs
        common :: Set State
        common = foldr1 Set.union $ map fst xs
