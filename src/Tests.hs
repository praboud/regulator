import Test.QuickCheck
import Text.Printf
import Text.ParserCombinators.Parsec (parse)
import Regulate

import Data.Maybe (isJust, fromMaybe)
import Data.List (intercalate, (\\), union)
import Data.Char (isPrint, isAscii, chr)
import Data.Ix (range)
import Control.Monad (liftM)

main :: IO ()
main = do
    title "match" >> quickCheckWith qcArgs (propMatch . fromMatch)
    title "mismatch" >> quickCheckWith qcArgs (propMismatch . fromMismatch)
    where
    title = printf "%-25s: "

qcArgs :: Args
qcArgs = stdArgs { maxSuccess = 1000 }

propMatch :: RegexCase -> Bool
propMatch = regexCaseMatches

propMismatch :: RegexCase -> Bool
propMismatch = not . regexCaseMatches

regexCaseMatches :: RegexCase -> Bool
regexCaseMatches (RegexCase reg str) = either (const False) (isJust . (flip accept str) . compileEnfaToDfa)
                                       $ parse regexpParser "regex" $ show reg

newtype MatchRegexCase = MatchRegexCase { fromMatch :: RegexCase } deriving Show
newtype MismatchRegexCase = MismatchRegexCase { fromMismatch :: RegexCase } deriving Show
data RegexCase = RegexCase Regex String deriving Show

instance Arbitrary MatchRegexCase where
    arbitrary = do
        regex <- arbitrary
        match <- matchedString regex
        return $ MatchRegexCase $ RegexCase regex match

instance Arbitrary MismatchRegexCase where
    arbitrary = do
        regex <- arbitrary
        match <- mismatchedString regex
        return $ MismatchRegexCase $ RegexCase regex match

matchedString :: Regex -> Gen String
matchedString (Simple ss) = mapM matchedChar ss
matchedString (Join Alternate rs) = oneof $ map matchedString rs
matchedString (Join Concat rs) = liftM concat $ mapM matchedString rs
matchedString (Mod Optional r) = oneof [return "", matchedString r]
matchedString (Mod Repeat0 r) = liftM concat $ listOf $ matchedString r
matchedString (Mod Repeat1 r) = liftM concat $ listOf1 $ matchedString r
matchedString (CharClass ss) = elements ss >>= liftM (:[]) . matchedChar

matchedChar :: Symbol -> Gen Char
matchedChar (Lit c) = return c
matchedChar (Class _ cs) = elements cs

mismatchedString :: Regex -> Gen String
mismatchedString (Simple ss) = some mismatchedChar matchedChar ss
mismatchedString (Join Alternate rs) = liftM (fromMaybe discard)
                                       $ suchThatMaybe (oneof $ map mismatchedString rs) noMatch
    where
    noMatch = (\str -> all (regexCaseMatches . flip RegexCase str) rs)
mismatchedString (Join Concat rs) = liftM concat $ some mismatchedString matchedString rs
mismatchedString (Mod op r) = case op of
    Optional -> str'
    Repeat0 -> liftM concat $ listOf1 str'
    Repeat1 -> liftM concat $ (if regexCaseMatches (RegexCase r "") then listOf1 else listOf) str'
    where
    str = mismatchedString r
    str' = suchThat str (not . null)
    -- where str = liftM (fromMaybe discard) $ suchThatMaybe (mismatchedString r) (regexCaseMatches . RegexCase (show r))
mismatchedString (CharClass ss) = liftM (:[]) $ elements $ range (chr 0, chr 255) \\ (foldr comb [] ss)
    where
    comb s ac = union ac $ case s of
        Lit c -> [c]
        Class _ cs -> cs

-- returns a list of b's, at least one of which comes from the mismatch generator
-- also randomly remove some elements, randomly add some elements
some :: Arbitrary b => (a -> Gen b) -> (a -> Gen b) -> [a] -> Gen [b]
some _ _ [] = listOf1 arbitrary
some f1 f2 as = some1 as
    where
    len = length as
    some1 [e] = liftM (:[]) $ f1 e
    some1 (e:es) = do
        r <- rand
        if r
            then do g <- f1 e; gs <- some0 es; return (g:gs)
            else do g <- f2 e; gs <- some1 es; return (g:gs)
    some0 = mapM (\e -> rand >>= (\r -> (if r then f1 else f2) e))
    rand = liftM (<1) $ choose (0, len)

mismatchedChar :: Symbol -> Gen Char
mismatchedChar (Lit c) = suchThat arbitrary (/=c)
mismatchedChar (Class _ cs) = case ls of
    [] -> discard
    _ -> elements ls
    where ls = (range (chr 0, chr 255)) \\ cs

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

-- instance Show Regex where
--     show (Simple ss) = concatMap show ss
--     show (Join op rs) = intercalate (show op) $ map (parens . show) rs
--     show (Mod op r) = (parens $ show r) ++ show op
--     show (CharClass ss) = '[' : concatMap show ss ++ "]"

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
