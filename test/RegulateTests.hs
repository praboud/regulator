import Test.QuickCheck
import Text.Printf
import Data.Array
import qualified Data.Set as Set
import Data.Maybe (isJust)

import RegulateTests.Match
import RegulateTests.Regex
import Regulate.Types (DFA(DFA))

main :: IO ()
main = do
    title "connected" >> qc (testRegexDfa propConnected :: Regex -> Bool)
    title "match" >> qc (propMatch . fromMatch)
    title "mismatch" >> qc (propMismatch . fromMismatch)
    where
    qc :: Testable t => t -> IO ()
    qc = quickCheckWith qcArgs
    title = printf "%-25s: "

qcArgs :: Args
qcArgs = stdArgs { maxSuccess = 1000 }

propMatch :: RegexCase -> Bool
propMatch = regexCaseMatches

propMismatch :: RegexCase -> Bool
propMismatch = not . regexCaseMatches

propConnected :: DFA -> Bool
propConnected (DFA ts q0 as) = hasOutbound && hasInbound
    where
    noInbound = foldr (\i ac -> maybe ac (`Set.delete` ac) (ts ! i)) (Set.delete q0 $ Set.fromList $ range (minQ, maxQ)) (range b)
    hasInbound = Set.empty == noInbound
    hasOutbound = all (\q -> Set.member q as || any (\s -> isJust $ ts ! (q, s)) (range (minS, maxS))) $ range (minQ, maxQ)
    b@((minQ, minS), (maxQ, maxS)) = bounds ts

newtype MatchRegexCase = MatchRegexCase { fromMatch :: RegexCase } deriving Show
newtype MismatchRegexCase = MismatchRegexCase { fromMismatch :: RegexCase } deriving Show

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
