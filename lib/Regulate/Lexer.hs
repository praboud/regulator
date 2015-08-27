{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Regulate.Lexer
    ( parseLexer
    , compileLexer
    , compileLexerEnfa
    , lexerTokenize
    ) where

import Text.ParserCombinators.Parsec hiding (optional, State)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Char (toUpper)

import Regulate.Types
import Regulate.Parse
import Regulate.Dfa
import Regulate.Enfa
import Regulate.Util

{- essentially the same idea as below, but assign meaningful names to the
 - tokens
 -}
lexerTokenize :: LexerDFA label -> [Symbol] -> Either String [Token label]
lexerTokenize (LexerDFA dfa names) = fmap (map (\(Token q s) -> Token (names Map.! q) s)) . tokenize dfa

parseLexer :: String -> Either String (LexerENFA String)
parseLexer = either (Left . show) Right . parse lexerParser "lexer"

compileLexer :: String -> Either String (LexerDFA String)
compileLexer = either Left (Right . (compileLexerEnfa "NIL" combine)) . parseLexer
    where combine = map toUpper . intercalate "_OR_" . Set.toList

lexerParser :: Parser (LexerENFA String)
lexerParser = sepEndBy1
    (do
        name <- many1 (char '_' <|> alphaNum)
        skipMany1 (oneOf " \t")
        enfa <- regexParser
        return (enfa, name))
    (char '\n')

compileLexerEnfa :: forall l m. Ord l => m -> (Set l -> m) -> LexerENFA l -> LexerDFA m
compileLexerEnfa nil precedence lexer = LexerDFA dfa $ Map.map getKind codeToState
    where
    (enfa, acceptNames) = foldl combine (emptyEnfa, Map.empty) lexer

    (dfa, codeToState) = compileEnfaToDfaExtra (Set.map (`Map.lookup` acceptNames)) enfa

    -- maps a set of enfa states (whose combination of states now represents
    -- a single dfa state) to a set of strings
    getKind :: Set State -> m
    getKind qs = if Set.null filt
        then nil
        else precedence filt
        where
        filt = setMapMaybe (`Map.lookup` acceptNames) qs'
        qs' = Set.foldr (\q qs'' -> Set.union qs'' $ epsilonClosure ts q) Set.empty qs

    combine :: (ENFA, Map Int l) -> (ENFA, l) -> (ENFA, Map Int l)
    combine (enfaAcc, names) (enfa', name) = (enfaAcc', names')
        where
        (xformAcc, xformSingle, enfaAcc') = alternateExtra enfaAcc enfa'
        -- NOTE: we assume that the transforms on the states are monotonic functions, for efficiency
        names' = Set.foldr (\a as -> Map.insert (xformSingle a) name as) (Map.mapKeysMonotonic xformAcc names) $ enfaAccept enfa'

{- general helpers -}

enfaAccept :: ENFA -> Set State
enfaAccept (ENFA _ _ as) = as
