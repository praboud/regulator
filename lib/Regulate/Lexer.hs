module Regulate.Lexer
    ( parseLexer
    , compileLexer
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
lexerTokenize :: LexerDFA -> [Symbol] -> Either String [Token String]
lexerTokenize (LexerDFA dfa names) = fmap (map (\(Token q s) -> Token (names Map.! q) s)) . tokenize dfa

parseLexer :: String -> Either String LexerENFA
parseLexer = either (Left . show) Right . parse lexerParser "lexer"

compileLexer :: String -> Either String LexerDFA
compileLexer = either Left (Right . compileLexerEnfa) . parseLexer

lexerParser :: Parser LexerENFA
lexerParser = sepEndBy1
    (do
        name <- many1 (char '_' <|> alphaNum)
        skipMany1 (oneOf " \t")
        enfa <- regexParser
        return (enfa, name))
    (char '\n')

compileLexerEnfa :: LexerENFA -> LexerDFA
compileLexerEnfa lexer = LexerDFA dfa $ Map.map getKind codeToState
    where
    (enfa, acceptNames) = foldl combine (ENFA Map.empty 0 Set.empty, Map.empty) lexer
    (ENFA ts _ _) = enfa

    (dfa, codeToState) = compileEnfaToDfaExtra (Set.map (`Map.lookup` acceptNames)) enfa

    -- maps a set of enfa states (whose combination of states now represents
    -- a single dfa state) to a set of strings
    getKind :: Set State -> String
    getKind qs = if Set.null filt
        then "NIL"
        else (map toUpper . intercalate "_OR_" . Set.toList) filt
        where
        filt = setMapMaybe (`Map.lookup` acceptNames) qs'
        qs' = Set.foldr (\q qs'' -> Set.union qs'' $ epsilonClosure ts q) Set.empty qs

    combine :: (ENFA, Map Int String) -> (ENFA, String) -> (ENFA, Map Int String)
    combine (enfaAcc, names) (enfa', name) = (enfaAcc', names')
        where
        (offsetAcc, offsetSingle, enfaAcc') = alternateExtra enfaAcc enfa'
        names' = Set.foldr (\a as -> Map.insert (a + offsetSingle) name as) (Map.mapKeysMonotonic (+offsetAcc) names) $ enfaAccept enfa'

{- general helpers -}

enfaAccept :: ENFA -> Set State
enfaAccept (ENFA _ _ as) = as
