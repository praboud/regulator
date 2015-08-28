module Regulate.Enfa
    ( repeat0
    , repeat1
    , optional
    , append
    , alternate
    , alternateExtra
    , alternateSingle
    , singletonEnfa
    , emptyEnfa
    , emptyEnfaAccept
    , enfaStateSet
    , epsilonClosure
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map

import Regulate.Types

{- ENFA combinators, used inside parser -}

-- given a language l, return language l*
repeat0 :: ENFA -> ENFA
repeat0 = optional . repeat1

-- given a language l, return language l+, formally: ll*
repeat1 :: ENFA -> ENFA
repeat1 (ENFA ts q0 as) = ENFA ts' q0 as
    where
    -- added transitions between all accept states and start
    ts' = Set.foldr (\a acc -> addTransition acc a Nothing q0) ts as

-- given a language l, return language l?, formally: ( epsilon | l )
optional :: ENFA -> ENFA
optional e@(ENFA ts q0 as) = ENFA (addTransition ts q0' Nothing q0) q0' (Set.insert q0' as)
    where
    -- we add a new dummy state in front of the ENFA, which becomes the new starting state.
    -- the state machine can take an epsilon-transition to the old starting state, or terminate
    -- here (since this state is an accepting state)
    -- this state also becomes an accept state
    q0' = Set.size $ enfaStateSet e

-- given languages l and m, return language lm
append :: ENFA -> ENFA -> ENFA
append e1@(ENFA ts q0 as) e2 = ENFA ts' q0 bs'
    where
    (ENFA us' r0' bs') = enfaIncreaseStates e2 $ fromIntegral $ Set.size $ enfaStateSet e1
    -- insert epsilon transitions between accept states of the first enfa, and the start state of the second
    ts' = Map.union us' $ Set.foldr (\a ts'' -> addTransition ts'' a Nothing r0') ts as

-- given languages l and m, return language l | m
alternate :: ENFA -> ENFA -> ENFA
alternate a b = (\(_,_,x) -> x) $ alternateExtra a b

-- as with alternate, but return the offsets that the respective ENFAs
-- were adjusted by during the computation. for use by the lexer, so even
-- after adjusting the states, we still know what state is what
alternateExtra :: ENFA -> ENFA -> (State -> State, State -> State, ENFA)
alternateExtra e1 e2 = (id, (+len1), ENFA ts' q0' as')
    where
    len1 = Set.size $ enfaStateSet e1
    len2 = Set.size $ enfaStateSet e2

    (ENFA ts r0 as) = e1
    (ENFA us s0 bs) = enfaIncreaseStates e2 len1

    q0' = len1 + len2
    as' = as `Set.union` bs
    -- we can insert without considering collisions since we've ensured that
    -- the states are all distinct
    ts' = Map.insert q0' (Map.singleton Nothing $ Set.fromList [r0, s0]) $ Map.union ts us

-- just a function for convenience / efficiency. this should be equivalent:
-- alternateSingle = foldr1 alternate $ map singletonEnfa
-- used to implement regexes like [abc]
alternateSingle :: [Symbol] -> ENFA
alternateSingle cs = ENFA ts q0 (Set.singleton q)
    where
    q0 = 0 -- starting state
    q = 1 -- accepting state
    ts = Map.singleton q0 (Map.fromList [(Just c, Set.singleton q) | c <- cs])

-- return a language which accepts exactly 1 character
singletonEnfa :: Symbol -> ENFA
singletonEnfa c = ENFA (Map.singleton q0 (Map.singleton (Just c) (Set.singleton q))) q0 (Set.singleton q)
    where
    q0 = 0 -- starting state
    q = 1 -- accepting state

-- enfa that accepts the empty string
emptyEnfaAccept :: ENFA
emptyEnfaAccept = ENFA Map.empty q0 (Set.singleton q0)
    where
    q0 = 0 -- starting state

-- enfa that accepts no strings
emptyEnfa :: ENFA
emptyEnfa = ENFA Map.empty q0 Set.empty
    where
    q0 = 0 -- starting state


{- Enfa helpers -}

addTransition :: ENFAMap -> State -> Maybe Symbol -> State -> ENFAMap
-- add a transition between the states q0 and q1 through c
addTransition ts q0 c q1 = Map.insertWith (Map.unionWith Set.union) q0 (Map.singleton c $ Set.singleton q1) ts

-- add ``n`` to all states in the ENFA (useful when preparing to merge two ENFAs)
enfaIncreaseStates :: ENFA -> State -> ENFA
enfaIncreaseStates (ENFA ts q0 as) n = ENFA ts' (q0 + n) as'
    where
    ts' = Map.map (Map.map (Set.mapMonotonic (+n))) $ Map.mapKeysMonotonic (+n) ts
    as' = Set.mapMonotonic (+n) as

enfaStateSet :: ENFA -> Set State
enfaStateSet (ENFA ts _ _) = Map.foldr (flip $ Map.foldr Set.union) (Map.keysSet ts) ts

-- return the states reachable from some set of states via epsilon transitions only
-- (including the state itself)
epsilonClosure :: ENFA -> Set State -> Set State
epsilonClosure (ENFA ts _ _) states = dfs states Set.empty
    where
    dfs :: Set State -> Set State -> Set State
    dfs qs knownClosure
        | Set.null qs = knownClosure
        | otherwise = dfs neighbours' (Set.union knownClosure qs)
        where
        neighbours = Set.foldr (\q ns -> maybe ns (Set.union ns) (Map.lookup q ts >>= Map.lookup Nothing)) Set.empty qs
        neighbours' = Set.difference neighbours knownClosure
