import Data.Maybe (isJust)
import Test.QuickCheck
import Text.Printf
import Text.ParserCombinators.Parsec (parse)
import Reg

import Data.List (intercalate)
import Data.Char (isPrint, isAscii)
import Control.Monad (liftM)

main :: IO ()
main = do
    title "parse" >> quickCheckWith qcArgs prop_parse
    title "match" >> quickCheckWith qcArgs prop_match
    where
    title = printf "%-25s: "

qcArgs :: Args
qcArgs = stdArgs { maxSuccess = 500 }

prop_parse :: Regex -> Bool
prop_parse r = either (const False) (const True) $ parse regexpParser "regex" $ show r
prop_match :: RegexCase -> Bool
prop_match = regexCaseMatches

regexCaseMatches :: RegexCase -> Bool
regexCaseMatches (RegexCase reg str) = either (const False) (isJust . (flip accept str) . compileEnfaToDfa)
                                       $ parse regexpParser "regex" reg

data RegexCase = RegexCase String String deriving Show

instance Arbitrary RegexCase where
    arbitrary = do
        regex <- arbitrary :: Gen Regex
        match <- matchingString regex
        return $ RegexCase (show regex) match
        where
        matchingString :: Regex -> Gen String
        matchingString (Simple ss) = mapM matchingChar ss
        matchingString (Join Alternate rs) = oneof $ map matchingString rs
        matchingString (Join Concat rs) = liftM concat $ mapM matchingString rs
        matchingString (Mod Optional r) = oneof [return "", matchingString r]
        matchingString (Mod Repeat0 r) = liftM concat $listOf $ matchingString r
        matchingString (Mod Repeat1 r) = liftM concat $listOf1 $ matchingString r
        matchingString (CharClass ss) = elements ss >>= liftM (:[]) . matchingChar

        matchingChar :: Symbol -> Gen Char
        matchingChar (Lit c) = return c
        matchingChar (Class _ cs) = elements cs

instance Arbitrary Regex where
    -- this does not represent the full domain of input regexes; potential problems:
    -- 1) overly simplistic handling of ^ and - within character classes
    -- 2) too many parentheses
    arbitrary = frequency gens
        where
        gens =
            [ (10, simple)
            , (1, join)
            , (1, modifier)
            , (1, charClass)
            ]
        char excl = suchThat (arbitrary :: Gen Char) (\x -> all ($x) [isPrint, isAscii, not . flip elem excl])
        simple =  liftM Simple $ listOf
                  $ oneof [liftM Lit (char "()[].\\+*|?") , liftM (uncurry Class) $ elements allCharacterClasses]
        join = do
            rs <- resize 5 $ listOf1 (arbitrary :: Gen Regex)
            op <- elements [Concat, Alternate]
            return $ Join op rs
        modifier = do
            r <- arbitrary :: Gen Regex
            op <- elements [Repeat0, Repeat1, Optional]
            return $ Mod op r
        charClass = liftM CharClass $ listOf1
                    $ oneof [liftM Lit (char "[]\\-^") , liftM (uncurry Class) $ elements characterClasses]

data Regex = Simple [Symbol]
           | Join JoinOp [Regex]
           | Mod ModOp Regex
           | CharClass [Symbol]

data ModOp = Repeat0 | Repeat1 | Optional
data JoinOp = Concat | Alternate
data Symbol = Lit Char | Class String String

instance Show Regex where
    show (Simple ss) = concatMap show ss
    show (Join op rs) = intercalate (show op) $ map (parens . show) rs
    show (Mod op r) = (parens $ show r) ++ show op
    show (CharClass ss) = '[' : concatMap show ss ++ "]"

instance Show ModOp where
    show Repeat0 = "*"
    show Repeat1 = "+"
    show Optional = "?"

instance Show JoinOp where
    show Concat = ""
    show Alternate = "|"

instance Show Symbol where
    show (Lit c) = [c]
    show (Class s _) = s

parens :: String -> String
parens s = '(' : s ++ ")"
