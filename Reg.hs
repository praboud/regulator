{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Reg
    ( regexpParser
    , lexerParser
    , compileEnfaToDfa
    , compileLexer
    , accept
    , tokenize
    , lexerTokenize
    , LexerDFA(LexerDFA)
    , DFA(DFA)
    ) where


import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array (Array, (!), array, bounds)
import Data.Ix (Ix, range, inRange)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, mapMaybe)
import Data.List (intercalate)
import Data.Char (toUpper, chr)

import Control.Monad (foldM, liftM, (>=>))
import Text.ParserCombinators.Parsec hiding (optional, State)


{- high level documentation things
 -
 - the objective is to generate DFA's from regexes (which can then be
 - used to quickly match regexes in linear time.
 - in particular, we want to automate the process of creating DFA's for
 - the lexers of compilers (such as the one I had to write for the
 - rather excellent CS241 course at the university of waterloo*).
 - to this end, we want to read in a list of regular expressions which
 - match to named tokens, and generate an array representing the dfa,
 - and the lookup table converting valid accept states to each
 - particular token. the output format should be a pair of c/c++ array
 - initializers.
 -
 - * as a side note, please do not use this to generate a compiler for
 -   CS241, or any other similar course. you will likely get in trouble
 -   when you cannot show you authored the code that generated the DFA
 -}

{- NOTES:
 - DFA:  deterministic finite automata
 -       At each state, each character either transitions to exactly 1
 -       state, or goes to an error state.
 -       very efficient and simple for the machine to verify whether a
 -       string is accepted by the DFA. However, writing code that
 -       stitches together DFA's is prohibitively complicated.
 -       ENFA's are used for this purpose.
 -
 - ENFA: episilon nondeterministic finite automata
 -       At each state, each character can transition to 0 or more
 -       states. (implicitly, if the character transitions to 0 state,
 -       it is said to transition to an error state. in the map, this
 -       can be represented by the character transitioning to an empty
 -       set of state, or the character having no transition at all. In
 -       practice, we should only ever have the second case.)
 -       A state can also transition to 0 or more state on the empty
 -       string (called the epsilon transition, represented by a
 -       transition through Nothing.)
 -       ENFA's are pretty simple to chain together (the main operations
 -       being alternation, repetition and concatenation). Therefore,
 -       we use ENFA's as an intermediate form between regular expressions
 -       and DFA's.
 -}

type State = Int
type Symbol = Char

data DFA = DFA (Array (State, Symbol) (Maybe State)) State (Set State) deriving Show

data ENFA = ENFA (Map State (Map (Maybe Symbol) (Set State))) State (Set State) deriving Show

type LexerENFA = [(ENFA, String)]

data LexerDFA = LexerDFA DFA (Map State String)

data Token x = Token x [Symbol]

instance Show (Token String) where
    show (Token name lexeme) = name ++ ": '" ++ lexeme ++ "'"

accept :: DFA -> [Symbol] -> Maybe State
accept (DFA ts q0 as) = foldM transition q0 >=> (\q -> if Set.member q as then Just q else Nothing)
    where
    transition q c = if inRange (bounds ts) (q, c) then ts ! (q, c) else Nothing

lexerTokenize :: LexerDFA -> [Symbol] -> Either String [Token String]
lexerTokenize (LexerDFA dfa names) cs = fmap (map (\(Token q s) -> Token (names Map.! q) s)) $ tokenize dfa cs

tokenize :: DFA -> [Symbol] -> Either String [Token Int]
tokenize (DFA ts q0 as) cs = tok_h [] q0 cs
    where
    err rs q
        | Set.member q as = Right $ Token q (reverse rs)
        | otherwise = Left "Error"
    tok_h rs q us
        | null us = fmap (\x -> [x]) $ err rs q
        | isNothing q' = do
            tok <- err rs q
            toks <- tok_h [] q0 us
            return (tok:toks)
        | otherwise = tok_h (u:rs) (fromJust q') (tail us)
        where
        u = head us
        q' = ts ! (q, u)

compileEnfaToDfa :: ENFA -> DFA
compileEnfaToDfa = fst . compileEnfaToDfaExtra

-- for clarity
--type DFAState = State
--type ENFAState = State

compileEnfaToDfaExtra :: ENFA -> (DFA, Map Int (Set State))
compileEnfaToDfaExtra (ENFA ts q0 as) = (DFA transitionArray (fromJust $ Map.lookup (Set.singleton q0) stateToCode) acceptStates, codeToState)
    where

    maxSym = maximum syms
    minSym = minimum syms
    syms = concat $ map (mapMaybe id . Map.keys) $ Map.elems ts

    transitionArray :: Array (State, Symbol) (Maybe State)
    transitionArray = array arrayBounds $ map (\(s, c) -> ((s, c), Map.lookup s codeToState >>= flip Map.lookup transitions >>= Map.lookup c >>= flip Map.lookup stateToCode)) $ range arrayBounds
    arrayBounds = ((0, minSym), (Map.size transitions - 1, maxSym))

    stateToCode :: Map (Set State) State
    stateToCode = foldr (\(qs, i) m -> Map.insert qs i m) Map.empty $ zip states [0..]
    codeToState :: Map Int (Set State)
    codeToState = foldr (\(qs, i) m -> Map.insert i qs m) Map.empty $ zip states [0..]

    transitions = buildTransitions Map.empty $ Set.singleton q0
    states = Set.toList $ Map.foldr (\v m -> Map.foldr Set.insert m v) (Map.keysSet transitions) transitions

    acceptStates :: Set State
    -- take accept states and add all states that can reach an accept state
    -- via an epsilon transition -- (ie: include the accept state in their epsilon closure)
    acceptStates = Map.foldrWithKey (\qs c ac -> if isAccept qs then Set.insert c ac else ac) Set.empty stateToCode
        where
        isAccept qs = any (overlap as . epsilonClosure ts) $ Set.toList qs
    overlap :: Set State -> Set State -> Bool
    overlap x y = not $ Set.null $ Set.intersection x y

    buildTransitions :: (Map (Set State) (Map Symbol (Set State))) -> (Set State) -> (Map (Set State) (Map Symbol (Set State)))
    buildTransitions ts' qs
        | Map.member qs ts' = ts' -- we have already encountered that state, do nothing
        | otherwise = Map.foldr (flip buildTransitions) (Map.insert qs neighbours ts') neighbours
        where
        equivalentqs :: Set State
        --equivalentqs = setCartesianProduct $ map (epsilonClosure ts) $ Set.elems qs
        equivalentqs = Set.foldr (\s ss -> Set.union ss $ epsilonClosure ts s) Set.empty qs

        nonEmptyTransitions :: State -> (Map Symbol (Set State))
        nonEmptyTransitions = maybe Map.empty (Map.mapKeysMonotonic fromJust . Map.filterWithKey (\k _ -> isJust k)) . flip Map.lookup ts

        neighbours :: Map Symbol (Set State)
        neighbours = Set.foldr (\q m -> Map.unionWith Set.union m $ nonEmptyTransitions q) Map.empty equivalentqs

        --Set.foldr Map.union$ Set.map (Set.map (Map.filterWithKey (\k _ -> isJust k) . flip Map.lookup ts) . epsilonClosure ts) qs

{- ENFA helpers, used by combinators and compiler -}

-- return the states reachable from some state via epsilon transitions only
epsilonClosure :: Map State (Map (Maybe Symbol) (Set State)) -> State -> Set State
epsilonClosure ts = epsilonClosure_h Set.empty
    where
    epsilonClosure_h nbrs q
        | Set.member q nbrs = nbrs -- we have already visited this node, we are done
        | otherwise = Set.foldr (flip epsilonClosure_h) (Set.insert q nbrs) adj
        where
        adj = fromMaybe Set.empty (Map.lookup q ts >>= Map.lookup Nothing)

enfaStateSet :: ENFA -> Set State
enfaStateSet (ENFA ts _ _) = Map.foldr (flip $ Map.foldr Set.union) (Map.keysSet ts) ts

enfaIncreaseStates :: ENFA -> State -> ENFA
enfaIncreaseStates (ENFA ts q0 as) n = ENFA ts' (q0 + n) as'
    where
    ts' = Map.map (Map.map (Set.mapMonotonic (+n))) $ Map.mapKeysMonotonic (+n) ts
    as' = Set.mapMonotonic (+n) as

addTransition :: (Ord c, Ord s) => (Map s (Map (Maybe c) (Set s))) -> s -> Maybe c -> s -> (Map s (Map (Maybe c) (Set s)))
addTransition ts q0 c q1 = Map.insertWith (Map.unionWith Set.union) q0 (Map.singleton c $ Set.singleton q1) ts

{- ENFA combinators, used inside parser -}

repeat0 :: ENFA -> ENFA
repeat0 e@(ENFA _ q0 as) = ENFA ts' q0 (Set.insert q0 as)
    where
    (ENFA ts' _ _) = repeat1 e

repeat1 :: ENFA -> ENFA
repeat1 (ENFA ts q0 as) = ENFA ts' q0 as
    where
    -- added transitions between accept states and start
    ts' = Set.foldr (\a acc -> addTransition acc a Nothing q0) ts as

optional :: ENFA -> ENFA
optional (ENFA ts q0 as) = ENFA ts q0 (Set.insert q0 as)

append :: ENFA -> ENFA -> ENFA
append e1@(ENFA ts q0 as) e2 = ENFA ts' q0 bs'
    where
    (ENFA us' r0' bs') = enfaIncreaseStates e2 $ fromIntegral $ Set.size $ enfaStateSet e1
    -- insert epsilon transitions between accept states of the first enfa, and the start state of the second
    ts' = Map.union us' $ Set.foldr (\a ts'' -> addTransition ts'' a Nothing r0') ts as

alternateExtra :: ENFA -> ENFA -> (State, State, ENFA)
alternateExtra e1 e2 = (offset1, offset2, ENFA vs 0 (Set.union as' bs'))
    where
    offset1 = 1
    offset2 = (+1) $ fromIntegral (Set.size $ enfaStateSet e1)
    (ENFA ts' q0' as') = enfaIncreaseStates e1 offset1
    (ENFA us' r0' bs') = enfaIncreaseStates e2 offset2
    vs = Map.insert 0 (Map.singleton Nothing $ Set.fromList [q0', r0']) $ (Map.union ts' us')

alternate :: ENFA -> ENFA -> ENFA
alternate a b = (\(_,_,x) -> x) $ alternateExtra a b

alternateSingle :: [Symbol] -> ENFA
alternateSingle cs = ENFA ts 0 (Set.singleton 1)
    where
    ts = Map.singleton 0 (foldr (\c a -> Map.insert (Just c) (Set.singleton 1) a) Map.empty cs)

singletonEnfa :: Symbol -> ENFA
singletonEnfa c = ENFA (Map.singleton 0 (Map.singleton (Just c) (Set.singleton 1))) 0 (Set.singleton 1)

-- emptyEnfa = ENFA Map.empty 0 (Set.singleton 0)

{- parser related things, turn string/regex into ENFA -}

regexpParser :: Parser ENFA
regexpParser = liftM (foldr1 alternate) $ sepBy1 regexpTermParser (char '|')
    where
    parens = between (char '(') (char ')') regexpParser
    -- parse a character range like [abc], or [a-zA-Z]
    -- alternate between any 1 single character
    charClassParser :: Parser ENFA
    charClassParser = do
        char '['
        invert <- optionMaybe $ char '^'
        cs <- liftM concat $ manyTill (try charRange <|> builtInRanges <|> singleChar) (char ']')
        return $ alternateSingle $ case invert of
            Nothing -> cs
            Just _  -> [x | x <- range (chr 0, chr 255), not $ Set.member x cs']
                where
                cs' = Set.fromList cs
    singleChar = liftM (:[]) $ escapeParser "[]"
    charRange = do
        lo <- anyChar
        char '-'
        hi <- anyChar
        return $ range (lo, hi)
    builtInRanges = charCode [('n',"\n"), ('t',"\t"), ('w', wordChars), ('s', spaceChars), ('d', digitChars)]
    charCode cs = char '\\' >> choice [char c >> return x | (c, x) <- cs]
    dot = char '.' >> return (range (chr 0, chr 255))

    -- gets postfix operators on regexes
    modifier :: ENFA -> Parser ENFA
    modifier enfa = do
        op <- combParser
        return $ op enfa
    combParser :: Parser (ENFA -> ENFA)
    combParser = (char '*' >> return repeat0)
                 <|> (char '+' >> return repeat1)
                 <|> (char '?' >> return optional)
                 <|> (return id)

    regexpTermParser = liftM (foldr1 append) $ many (
            (try parens
             <|> try charClassParser
             <|> try (liftM alternateSingle (builtInRanges <|> dot))
             <|> (liftM singletonEnfa $ escapeParser "|()[]+?*.")
            ) >>= modifier)

spaceChars :: String
spaceChars = " \t\n"
digitChars :: String
digitChars = "0123456789"
wordChars :: String
wordChars  = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

escapeParser :: [Char] -> Parser Char
-- parses any character, except unescaped versions of any character in
-- the list provided
-- nb: we never allow newlines
-- should probably extend this to any non-printable
escapeParser cs = (char esc >> oneOf (esc : cs)) <|> (noneOf ('\n' : cs))
    where esc = '\\'

{- things dealing with language lexers -}

lexerParser :: Parser LexerENFA
lexerParser = sepEndBy1
    (do
        name <- many1 alphaNum
        skipMany1 space
        enfa <- regexpParser
        return (enfa, name))
    (char '\n')

compileLexer :: LexerENFA -> LexerDFA
-- compileLexer toks = trace ((unlines $ map show toks) ++ show enfa ++ "\n" ++ show acceptNames ++ "\n" ++ show codeToState) $ LexerDFA dfa $ Map.map getKind codeToState
compileLexer toks = LexerDFA dfa $ Map.map getKind codeToState
    where
    (enfa, acceptNames) = foldl combine (ENFA Map.empty 0 Set.empty, Map.empty) toks
    (ENFA ts _ _) = enfa

    (dfa, codeToState) = compileEnfaToDfaExtra enfa

    -- maps a set of enfa states (whose combination of states now represents
    -- a single dfa state) to a set of strings
    getKind :: Set State -> String
    getKind qs = if Set.null filt
        then "NIL"
        else (map toUpper . intercalate "_OR_" . Set.toList) filt
        where
        filt = setMapMaybe (flip Map.lookup acceptNames) qs'
        qs' = Set.foldr (\q qs'' -> Set.union qs'' $ epsilonClosure ts q) Set.empty qs

    combine :: (ENFA, Map Int String) -> (ENFA, String) -> (ENFA, Map Int String)
    combine (enfaAcc, names) (enfa', name) = (enfaAcc', names')
        where
        (offsetAcc, offsetSingle, enfaAcc') = alternateExtra enfaAcc enfa'
        names' = Set.foldr (\a as -> Map.insert (a + offsetSingle) name as) (Map.mapKeysMonotonic (+offsetAcc) names) $ enfaAccept enfa'

{- general helpers -}

setMapMaybe :: (Ord x, Ord y) => (x -> Maybe y) -> Set x -> Set y
setMapMaybe f = Set.foldr (\x a -> case f x of
    Nothing -> a
    Just y  -> Set.insert y a) Set.empty

enfaAccept :: ENFA -> (Set State)
enfaAccept (ENFA _ _ as) = as
