import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array (Array, (!), array)
import Data.Ix (Ix, range)
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe)

import Control.Monad (foldM)

data DFA c s = DFA (Array (s, c) (Maybe s)) s (Set s) deriving Show

accept :: (Ix c, Ix s) => DFA c s -> [c] -> Bool
accept (DFA ts q0 as) = maybe False (flip Set.member as) . foldM transition q0
    where
    transition q c = ts ! (q, c)

data ENFA c s = ENFA (Map s (Map (Maybe c) (Set s))) s (Set s) deriving Show

compileEnfaToDfa :: forall c s. (Ix c, Ix s) => ENFA c s -> DFA c Int
compileEnfaToDfa (ENFA ts q0 as) = DFA transitionArray (fromJust $ Map.lookup (Set.singleton q0) stateToCode) acceptStates
    where
    maxSym = maximum syms
    minSym = minimum syms
    syms = concat $ map (mapMaybe id . Map.keys) $ Map.elems ts

    transitionArray :: Array (Int, c) (Maybe Int)
    transitionArray = array arrayBounds $ map (\(s, c) -> ((s, c), Map.lookup s codeToState >>= flip Map.lookup transitions >>= Map.lookup c >>= flip Map.lookup stateToCode)) $ range arrayBounds
    arrayBounds = ((0, minSym), (Map.size transitions - 1, maxSym))

    stateToCode :: Map (Set s) Int
    stateToCode = foldr (\(qs, i) m -> Map.insert qs i m) Map.empty $ zip states [0..]
    codeToState :: Map Int (Set s)
    codeToState = foldr (\(qs, i) m -> Map.insert i qs m) Map.empty $ zip states [0..]

    transitions = buildTransitions Map.empty $ Set.singleton q0
    states = Map.keys transitions

    acceptStates :: Set Int
    acceptStates = Set.foldr (\a ac -> Set.union ac $ Set.fromList $ Map.elems $ Map.filterWithKey (\k _ -> overlap k $ reverseEpsilonClosure ts a) stateToCode) Set.empty as
    overlap :: Set s -> Set s -> Bool
    overlap x y = Set.null $ Set.intersection x y

    buildTransitions :: (Map (Set s) (Map c (Set s))) -> (Set s) -> (Map (Set s) (Map c (Set s)))
    buildTransitions ts' qs
        | Map.member qs ts' = ts' -- we have already encountered that state, do nothing
        | otherwise = Map.foldr (flip buildTransitions) (Map.insert qs neighbours ts') neighbours
        where
        equivalentqs :: Set s
        --equivalentqs = setCartesianProduct $ map (epsilonClosure ts) $ Set.elems qs
        equivalentqs = Set.foldr (\s ss -> Set.union ss $ epsilonClosure ts s) Set.empty qs

        nonEmptyTransitions :: s -> (Map c (Set s))
        nonEmptyTransitions = maybe Map.empty (Map.mapKeysMonotonic fromJust . Map.filterWithKey (\k _ -> isJust k)) . flip Map.lookup ts

        neighbours :: Map c (Set s)
        neighbours = Set.foldr (\q m -> Map.unionWith Set.union m $ nonEmptyTransitions q) Map.empty equivalentqs
        
        --Set.foldr Map.union$ Set.map (Set.map (Map.filterWithKey (\k _ -> isJust k) . flip Map.lookup ts) . epsilonClosure ts) qs

epsilonClosure :: (Ord s, Ord c) => (Map s (Map (Maybe c) (Set s))) -> s -> Set s
epsilonClosure ts = epsilonClosure_h Set.empty
    where
    epsilonClosure_h nbrs q
        | Set.member q nbrs = nbrs -- we have already visited this node, we are done
        | otherwise = Set.foldr (flip epsilonClosure_h) (Set.insert q nbrs) adj
        where
        adj = fromMaybe Set.empty (Map.lookup q ts >>= Map.lookup Nothing)
        nbrs' = Set.insert q nbrs

reverseEpsilonClosure :: forall s c. (Ord s, Ord c) => (Map s (Map (Maybe c) (Set s))) -> s -> Set s
reverseEpsilonClosure ts = reverseEpsilonClosure_h Set.empty
    where
    reverseEpsilonClosure_h :: (Set s) -> s -> (Set s)
    reverseEpsilonClosure_h sources q
        | Set.member q sources = sources
        | otherwise = Set.foldr (flip reverseEpsilonClosure_h) sources' sources'
        where
        sources' :: Set s
        sources' = Map.foldrWithKey (\state statetrans src -> if fromMaybe False (Map.lookup Nothing statetrans >>= return . Set.member q) then Set.insert state src else src) Set.empty ts

main = return ()
