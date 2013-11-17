module RegulateTests.Regex where

import Test.QuickCheck
import Control.Monad (liftM)
import Data.Char (isPrint, isAscii)
import Regulate (DFA, allCharacterClasses, characterClasses, regexParse, compileEnfaToDfa)
import Data.List (intercalate)

toDfa :: Show a => a -> Maybe DFA
toDfa = either (const Nothing) (Just . compileEnfaToDfa) . regexParse . show

testRegexDfa :: Show a => (DFA -> Bool) -> a -> Bool
testRegexDfa f = maybe False f . toDfa

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
