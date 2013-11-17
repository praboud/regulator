module RegulateTests.Match
    ( regexCaseMatches
    , matchedString
    , mismatchedString
    , RegexCase(RegexCase)
    ) where

import RegulateTests.Regex
import Test.QuickCheck
import Control.Monad (liftM)
import Data.Maybe (isJust, fromMaybe)
import Regulate
import Data.List ((\\), union)
import Data.Char (chr)
import Data.Ix (range)
import Text.Printf (printf)

data RegexCase = RegexCase Regex String

instance Show RegexCase where
    show (RegexCase reg str) = printf "matching \"%s\" against \"%s\"" (show reg) str

regexCaseMatches :: RegexCase -> Bool
regexCaseMatches (RegexCase reg match) = regexMatches reg match

regexMatches :: (Show a) => a -> String -> Bool
regexMatches regex match = testRegexDfa (isJust . flip accept match) regex

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
    noMatch str = all (`regexMatches` str) rs
mismatchedString (Join Concat rs) = liftM concat $ some mismatchedString matchedString rs
mismatchedString (Mod op r) = case op of
    Optional -> str'
    Repeat0 -> liftM concat $ listOf1 str'
    Repeat1 -> liftM concat $ (if regexMatches r "" then listOf1 else listOf) str'
    where
    str = mismatchedString r
    str' = suchThat str (not . null)
    -- where str = liftM (fromMaybe discard) $ suchThatMaybe (mismatchedString r) (regexCaseMatches . RegexCase (show r))
mismatchedString (CharClass ss) = liftM (:[]) $ elements $ range (chr 0, chr 255) \\ foldr comb [] ss
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
    some1 [] = undefined
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
    where ls = range (chr 0, chr 255) \\ cs
