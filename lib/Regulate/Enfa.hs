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
import Data.Maybe (fromMaybe)

import Regulate.Types

{- ENFA combinators, used inside parser -}

-- given a language l, return language l*
repeat0 :: ENFA -> ENFA
repeat0 e@(ENFA _ q0 as) = ENFA ts' q0 (Set.insert q0 as)
    where
    (ENFA ts' _ _) = repeat1 e

-- given a language l, return language l+, formally: ll*
repeat1 :: ENFA -> ENFA
repeat1 (ENFA ts q0 as) = ENFA ts' q0 as
    where
    -- added transitions between accept states and start
    ts' = Set.foldr (\a acc -> addTransition acc a Nothing q0) ts as

-- given a language l, return language l?, formally: ( epsilon | l )
optional :: ENFA -> ENFA
optional (ENFA ts q0 as) = ENFA ts q0 (Set.insert q0 as)

-- given languages l and m, return language lm
append :: ENFA -> ENFA -> ENFA
append e1@(ENFA ts q0 as) e2 = ENFA ts' q0 bs'
    where
    (ENFA us' r0' bs') = enfaIncreaseStates e2 $ fromIntegral $ Set.size $ enfaStateSet e1
    -- insert epsilon transitions between accept states of the first enfa, and the start state of the second
    ts' = Map.union us' $ Set.foldr (\a ts'' -> addTransition ts'' a Nothing r0') ts as

-- as with alternate, but return the offsets that the respective ENFAs
-- were adjusted by during the computation. for use by the lexer, so even
-- after adjusting the states, we still know what state is what
alternateExtra :: ENFA -> ENFA -> (Int, Int, ENFA)
alternateExtra e1 e2 = (offset1, offset2, ENFA vs 0 (Set.union as' bs'))
    where
    offset1 = 1
    offset2 = (+1) $ fromIntegral (Set.size $ enfaStateSet e1)
    (ENFA ts' q0' as') = enfaIncreaseStates e1 offset1
    (ENFA us' r0' bs') = enfaIncreaseStates e2 offset2
    vs = Map.insert 0 (Map.singleton Nothing $ Set.fromList [q0', r0']) $ Map.union ts' us'

-- given languages l and m, return language l | m
alternate :: ENFA -> ENFA -> ENFA
alternate a b = (\(_,_,x) -> x) $ alternateExtra a b

-- just a function for convenience / efficiency. this should be equivalent:
-- alternateSingle = foldr1 alternate $ map singletonEnfa
-- used to implement regexes like [abc]
alternateSingle :: [Symbol] -> ENFA
alternateSingle cs = ENFA ts 0 (Set.singleton 1)
    where
    ts = Map.singleton 0 (foldr (\c a -> Map.insert (Just c) (Set.singleton 1) a) Map.empty cs)

-- return a language which accepts exactly 1 character
singletonEnfa :: Symbol -> ENFA
singletonEnfa c = ENFA (Map.singleton 0 (Map.singleton (Just c) (Set.singleton 1))) 0 (Set.singleton 1)

-- enfa that accepts the empty string
emptyEnfaAccept :: ENFA
emptyEnfaAccept = ENFA Map.empty 0 (Set.singleton 0)

-- enfa that accepts no strings
emptyEnfa :: ENFA
emptyEnfa = ENFA Map.empty 0 Set.empty


{- Enfa helpers -}

addTransition :: ENFAMap -> State -> Maybe Symbol -> State -> ENFAMap
addTransition ts q0 c q1 = Map.insertWith (Map.unionWith Set.union) q0 (Map.singleton c $ Set.singleton q1) ts

-- add ``n`` to all states in the ENFA (useful when preparing to merge two ENFAs)
enfaIncreaseStates :: ENFA -> State -> ENFA
enfaIncreaseStates (ENFA ts q0 as) n = ENFA ts' (q0 + n) as'
    where
    ts' = Map.map (Map.map (Set.mapMonotonic (+n))) $ Map.mapKeysMonotonic (+n) ts
    as' = Set.mapMonotonic (+n) as

enfaStateSet :: ENFA -> Set State
enfaStateSet (ENFA ts _ _) = Map.foldr (flip $ Map.foldr Set.union) (Map.keysSet ts) ts

-- return the states reachable from some state via epsilon transitions only
epsilonClosure :: ENFAMap -> State -> Set State
epsilonClosure ts = epsilonClosure_h Set.empty
    where
    epsilonClosure_h nbrs q
        | Set.member q nbrs = nbrs -- we have already visited this node, we are done
        | otherwise = Set.foldr (flip epsilonClosure_h) (Set.insert q nbrs) adj
        where
        adj = fromMaybe Set.empty (Map.lookup q ts >>= Map.lookup Nothing)
