import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array (Array, (!), array, bounds, listArray)
import Data.Ix (Ix, range, inRange)
import Data.Maybe (fromJust, isJust, fromMaybe, mapMaybe)
import Data.List (foldr1, intercalate)
import Data.Char (toUpper, ord, chr)

import Control.Monad (foldM, liftM)
import Text.ParserCombinators.Parsec hiding (optional)
import Text.Printf (printf)

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
 -
 - ENFA's and DFA's are somewhat written to accomodate any ordinal
 - type as the state type. in the vast majority of cases, we actually just use the int
 - type in practice. In some places in the code, I have used int's for convenience.
 - In the future, I should probably make all of this code generic, or just bite the bullet
 - and make all of the code require integers. This half-half solution is too confusing.
 -}

data DFA c s = DFA (Array (s, c) (Maybe s)) s (Set s) deriving Show

data ENFA c s = ENFA (Map s (Map (Maybe c) (Set s))) s (Set s) deriving Show

type LexerENFA c s = [(ENFA c s, String)]

data LexerDFA c s = LexerDFA (DFA c s) (Array s String)

instance Show (LexerDFA Char Int) where
    show (LexerDFA (DFA ts q0 _) as) = header ++ q0' ++ ts'
        where
        err = -1 :: Int
        header = printf "#define ST_ERR %d\n" err
        q0' = printf "#define ST_START %d\n" q0

        ts' = (printf "int transitions[%d][%d] {\n" (max_st - min_st + 1) ((ord max_char) - (ord min_char) + 1)) ++ unlines tlines ++ "};\n"
        tlines = [intercalate ", " [gettr s c | c <- range (min_char, max_char)] | s <- range (min_st, max_st)]
        gettr s c = show $ fromMaybe err $ if inRange (bounds ts) (s, c) then ts ! (s, c) else Nothing

        ((min_st, _), (max_st, _)) = bounds ts
        min_char = chr 0
        max_char = chr 255

accept :: (Ix c, Ix s) => DFA c s -> [c] -> Bool
accept (DFA ts q0 as) = maybe False (flip Set.member as) . foldM transition q0
    where
    transition q c = if inRange (bounds ts) (q, c) then ts ! (q, c) else Nothing

compileEnfaToDfa :: (Ix c, Ord s) => ENFA c s -> DFA c Int
compileEnfaToDfa = fst . compileEnfaToDfaExtra

compileEnfaToDfaExtra :: forall c s. (Ix c, Ord s) => ENFA c s -> (DFA c Int, Map Int (Set s))
compileEnfaToDfaExtra (ENFA ts q0 as) = (DFA transitionArray (fromJust $ Map.lookup (Set.singleton q0) stateToCode) acceptStates, codeToState)
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
    overlap x y = not $ Set.null $ Set.intersection x y

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

{- ENFA helpers, used by combinators and compiler -}

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

addTransition :: (Ord c, Ord s) => (Map s (Map (Maybe c) (Set s))) -> s -> Maybe c -> s -> (Map s (Map (Maybe c) (Set s)))
addTransition ts q0 c q1 = Map.insertWith (Map.unionWith Set.union) q0 (Map.singleton c $ Set.singleton q1) ts

{- ENFA combinators, used inside parser -}

repeat0 :: (Ord c, Ord s) => ENFA c s -> ENFA c s
repeat0 e@(ENFA ts q0 as) = ENFA ts' q0 (Set.insert q0 as)
    where
    (ENFA ts' _ _) = repeat1 e

repeat1 :: (Ord c, Ord s) => ENFA c s -> ENFA c s
repeat1 (ENFA ts q0 as) = ENFA ts' q0 as
    where
    -- added transitions between accept states and start
    ts' = Set.foldr (\a acc -> addTransition acc a Nothing q0) ts as

optional :: (Ord c, Ord s) => ENFA c s -> ENFA c s
optional (ENFA ts q0 as) = ENFA ts q0 (Set.insert q0 as)

append :: (Ord c, Integral s) => ENFA c s -> ENFA c s -> ENFA c s
append fst@(ENFA ts q0 as) snd = ENFA ts' q0 bs'
    where
    (ENFA us' r0' bs') = enfaIncreaseStates snd $ fromIntegral $ Set.size $ enfaStateSet fst
    -- insert epsilon transitions between accept states of the first enfa, and the start state of the second
    ts' = Map.union us' $ Set.foldr (\a ts' -> addTransition ts' a Nothing r0') ts as

alternateExtra :: (Ord c, Integral s) => ENFA c s -> ENFA c s -> (s, s, ENFA c s)
alternateExtra fst snd = (offsetFst, offsetSnd, ENFA vs 0 (Set.union as' bs'))
    where
    offsetFst = 1
    offsetSnd = (+1) $ fromIntegral (Set.size $ enfaStateSet fst)
    (ENFA ts' q0' as') = enfaIncreaseStates fst offsetFst
    (ENFA us' r0' bs') = enfaIncreaseStates snd offsetSnd
    vs = Map.insert 0 (Map.singleton Nothing $ Set.fromList [q0', r0']) $ (Map.union ts' us')

alternate a b = (\(_,_,x) -> x) $ alternateExtra a b

alternateSingle :: (Ord c) => [c] -> ENFA c Int
alternateSingle cs = ENFA ts 0 (Set.singleton 1)
    where
    ts = Map.singleton 0 (foldr (\c a -> Map.insert (Just c) (Set.singleton 1) a) Map.empty cs)

singletonEnfa :: Ord c => c -> ENFA c Int
singletonEnfa c = ENFA (Map.singleton 0 (Map.singleton (Just c) (Set.singleton 1))) 0 (Set.singleton 1)

emptyEnfa = ENFA Map.empty 0 (Set.singleton 0)


{- parser related things, turn string/regex into ENFA -}

regexpParser :: Parser (ENFA Char Int)
regexpParser = liftM (foldr1 alternate) $ sepBy1 regexpTermParser (char '|')
    where
    parens = between (char '(') (char ')') regexpParser
    -- parse a character range like [abc], or [a-zA-Z]
    -- alternate between any 1 single character
    charClassParser :: Parser (ENFA Char Int)
    charClassParser = liftM (alternateSingle . concat) (char '[' >> manyTill (try charRange <|> singleChar) (char ']'))
    singleChar = liftM (:[]) anyChar
    charRange = do
        lo <- anyChar
        char '-'
        hi <- anyChar
        return $ range (lo, hi)

    -- gets postfix operators on regexes
    modifier :: ENFA Char Int -> Parser (ENFA Char Int)
    modifier enfa = do
        op <- combParser
        return $ op enfa
    combParser :: (Ord c, Ord s) => Parser (ENFA c s -> ENFA c s)
    combParser = (char '*' >> return repeat0)
                 <|> (char '+' >> return repeat1)
                 <|> (char '?' >> return optional)
                 <|> (return id)

    regexpTermParser = liftM (foldr1 append) $ many ((parens <|> charClassParser <|> (liftM singletonEnfa $ escapeParser "|()*")) >>= modifier)

escapeParser :: [Char] -> Parser Char
-- parses any character, except unescaped versions of any character in
-- the list provided
-- nb: we never allow newlines
-- should probably extend this to any non-printable
escapeParser cs = (char esc >> oneOf (esc : cs)) <|> (noneOf ('\n' : cs))
    where esc = '\\'

{- things dealing with language lexers -}

lexerParser :: Parser (LexerENFA Char Int)
lexerParser = sepEndBy1
    (do
        name <- many1 alphaNum
        skipMany1 space
        enfa <- regexpParser
        return (enfa, name))
    (char '\n')

compileLexer :: forall c. Ix c => LexerENFA c Int -> LexerDFA c Int
compileLexer toks = LexerDFA dfa stateArray
    where
    (enfa, acceptNames) = foldl combine (emptyEnfa, Map.empty) toks

    (dfa, codeToState) = compileEnfaToDfaExtra enfa

    stateArray = funcToArray (0, fst $ Map.findMax codeToState) getKind

    getKind :: Int -> String
    getKind = maybe "" getKind_h . flip Map.lookup codeToState
        where
        getKind_h i = if Set.null filt
            then "ERR"
            else (map toUpper . intercalate "_OR_" . Set.toList) filt
            where filt = setMapMaybe (flip Map.lookup acceptNames) i

    combine :: (ENFA c Int, Map Int String) -> (ENFA c Int, String) -> (ENFA c Int, Map Int String)
    combine (enfaAcc, names) (enfa, name) = (enfaAcc', names')
        where
        (offsetAcc, offsetSingle, enfaAcc') = alternateExtra enfaAcc enfa
        names' = Set.foldr (\a as -> Map.insert (a + offsetSingle) name as) (Map.mapKeysMonotonic (+offsetAcc) names) $ enfaAccept enfa

{- main io shit -}
main = do
    regex <- getLine
    case parse regexpParser "regex" regex of
        Right enfa -> print enfa >> getContents >>= (mapM_ (print . accept dfa) . lines)
            where dfa = compileEnfaToDfa enfa
        Left err -> print err

{-
main = do
    contents <- getContents
    case parse lexerParser "lexer" contents of
        Right lexer -> print $ compileLexer lexer
        Left err -> print err
-}

{- general helpers -}

funcToArray :: Ix i => (i, i) -> (i -> x) -> Array i x
funcToArray bound f = listArray bound $ map f $ range bound

setMapMaybe :: (Ord x, Ord y) => (x -> Maybe y) -> Set x -> Set y
setMapMaybe f = Set.foldr (\x a -> case f x of
    Nothing -> a
    Just y  -> Set.insert y a) Set.empty

enfaTransitions (ENFA ts _ _) = ts
enfaAccept (ENFA _ _ as) = as

{-
fromRight (Right v) = v

test = fromRight $ parse regexpParser "regex" "aoeu|asdf"
-}
