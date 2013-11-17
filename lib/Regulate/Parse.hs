module Regulate.Parse
    ( parseRegex
    , regexParser
    , alternateExtra
    , enfaStateSet
    , characterClasses
    , allCharacterClasses
    ) where

import qualified Data.Set as Set

import Data.Ix (range)
import Data.Char (chr)

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (optional, State)
import Regulate.Types
import Regulate.Enfa

{- parser related things, turn string/regex into ENFA -}

parseRegex :: String -> Either String ENFA
parseRegex = either (Left . show) Right . parse regexParser "regex"

regexParser :: Parser ENFA
regexParser = liftM (foldr alternate emptyEnfa) $ sepBy1 regexpTermParser (char '|')
    where
    parens = between (char '(') (char ')') regexParser
    -- parse a character range like [abc], or [a-zA-Z]
    -- alternate between any 1 single character
    charClassParser :: Parser ENFA
    charClassParser = do
        char '['
        invert <- optionMaybe $ char '^'
        cs <- liftM concat $ manyTill (try charRange <|> classes characterClasses <|> singleChar) (char ']')
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

    -- gets postfix operators on regexes
    modifier :: ENFA -> Parser ENFA
    modifier enfa = do
        op <- combParser
        return $ op enfa
    combParser :: Parser (ENFA -> ENFA)
    combParser = (char '*' >> return repeat0)
                 <|> (char '+' >> return repeat1)
                 <|> (char '?' >> return optional)
                 <|> return id

    classes = choice . map (\(code, cls) -> try (string code) >> return cls)

    regexpTermParser = liftM (foldr append emptyEnfaAccept) $ many (
            (try parens
             <|> try charClassParser
             <|> try (liftM alternateSingle $ classes allCharacterClasses)
             <|> liftM singletonEnfa (escapeParser "|()[]+?*.")
            ) >>= modifier)

allCharacterClasses :: [(String, String)]
allCharacterClasses = (".", range (chr 0, chr 127)) : characterClasses

characterClasses :: [(String, String)]
characterClasses =
    [ ("\\n", "\n")
    , ("\\t", "\t")
    , ("\\s", " \t\n")
    , ("\\w", "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
    , ("\\d", "0123456789")
    ]

escapeParser :: String -> Parser Char
-- parses any character, except unescaped versions of any character in
-- the list provided
escapeParser cs = (char esc >> oneOf (esc : cs)) <|> noneOf cs
    where esc = '\\'

{- things dealing with language lexers -}

