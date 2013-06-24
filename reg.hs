import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array (Array, (!), array, bounds)
import Data.Ix (Ix, range, inRange)
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe)
import Data.List (foldr1)

import Control.Monad (foldM, liftM)
import Text.ParserCombinators.Parsec

data DFA c s = DFA (Array (s, c) (Maybe s)) s (Set s) deriving Show

accept :: (Ix c, Ix s) => DFA c s -> [c] -> Bool
accept (DFA ts q0 as) = maybe False (flip Set.member as) . foldM transition q0
    where
    transition q c = if inRange (bounds ts) (q, c) then ts ! (q, c) else Nothing

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
    states = Set.toList $ Map.foldr (\v m -> Map.foldr Set.insert m v) (Map.keysSet transitions) transitions

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
reverseEpsilonClosure ts q0 = Set.insert q0 $ reverseEpsilonClosure_h Set.empty q0
    where
    reverseEpsilonClosure_h :: (Set s) -> s -> (Set s)
    reverseEpsilonClosure_h sources q
        -- if we have seen this node before, stop (otherwise, we will encounter a cycle)
        | Set.member q sources = sources
        | otherwise = Set.foldr (flip reverseEpsilonClosure_h) sources' sources'
        where
        sources' :: Set s
        -- a set of sources
        sources' = Map.foldrWithKey (\state statetrans src -> if maybe False (Set.member q) (Map.lookup Nothing statetrans) then Set.insert state src else src) sources ts

enfaStateSet :: (Ord c, Ord s) => ENFA c s -> Set s
enfaStateSet (ENFA ts _ _) = Map.foldr (flip $ Map.foldr Set.union) (Map.keysSet ts) ts

enfaIncreaseStates :: (Ord c, Integral s) => ENFA c s -> s -> ENFA c s
enfaIncreaseStates (ENFA ts q0 as) n = ENFA ts' (q0 + n) as'
    where
    ts' = Map.map (Map.map (Set.map (+n))) $ Map.mapKeysMonotonic (+n) ts
    as' = Set.map (+n) as

repeat :: (Ord c, Ord s) => ENFA c s -> ENFA c s
repeat (ENFA ts q0 as) = ENFA ts' q0 (Set.insert q0 as)
    where
    -- added transitions between accept states and start
    ts' = Set.foldr (\a -> Map.insertWith (Map.unionWith Set.union) a (Map.singleton Nothing $ Set.singleton q0)) ts as

append :: (Ord c, Integral s) => ENFA c s -> ENFA c s -> ENFA c s
append fst@(ENFA ts q0 as) snd = ENFA ts' q0 bs'
    where
    (ENFA us' r0' bs') = enfaIncreaseStates snd $ fromIntegral $ Set.size $ enfaStateSet fst
    -- insert epsilon transitions between accept states of the first enfa, and the start state of the second
    ts' = Map.union us' $ Set.foldr (\a ts' -> Map.insertWith (Map.unionWith Set.union) a (Map.singleton Nothing $ Set.singleton r0') ts') ts as

alternate :: (Ord c, Integral s) => ENFA c s -> ENFA c s -> ENFA c s
alternate fst snd = ENFA vs 0 (Set.union as' bs')
    where
    (ENFA ts' q0' as') = enfaIncreaseStates fst 1
    (ENFA us' r0' bs') = enfaIncreaseStates snd $ (+1) $ fromIntegral (Set.size $ enfaStateSet fst)
    vs = Map.insert 0 (Map.singleton Nothing $ Set.fromList [q0', r0']) $ (Map.union ts' us')

singletonEnfa :: Ord c => c -> ENFA c Int
singletonEnfa c = ENFA (Map.singleton 0 (Map.singleton (Just c) (Set.singleton 1))) 0 (Set.singleton 1)

emptyEnfa = ENFA Map.empty 0 (Set.singleton 0)

regexpParser :: Parser (ENFA Char Int)
regexpParser = liftM (foldr1 alternate) $ sepBy1 regexpTermParser (char '|')
    where
    parens = do
        char '('
        r <- regexpParser
        char ')'
        op <- optionMaybe (char '*')
        return $ case op of
            Nothing -> r
            Just '*' -> Main.repeat r
    regexpTermParser = liftM (foldr1 append) $ many (parens <|> (liftM singletonEnfa anyChar))

-- DEBUG CODE
{-
main = do
    regex <- getLine
    case liftM compileEnfaToDfa (parse regexpParser "regex" regex) of
        Right dfa -> getContents >>= (mapM_ (print . accept dfa) . lines)
        Left err -> print err
-}

enfaTransitions (ENFA ts _ _)= ts

fromRight (Right v) = v

test = fromRight $ parse regexpParser "regex" "aoeu"
