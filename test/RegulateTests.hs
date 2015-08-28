import Test.QuickCheck
import Text.Printf

import Data.Array
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import RegulateTests.Match
import RegulateTests.Regex
import Regulate.Types (DFA(DFA), LexerDFA, Token(Token))
import Regulate.Lexer

main :: IO ()
main = do
    title "connected" >> qc (testRegexDfa propConnected :: Regex -> Bool)
    title "match" >> qc (propMatch . fromMatch)
    title "mismatch" >> qc (propMismatch . fromMismatch)
    title "lexer match" >> quickCheck propLexerTokenize
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
    hasInbound = Set.null noInbound
    hasOutbound = all (\q -> Set.member q as || any (\s -> isJust $ ts ! (q, s)) (range (minS, maxS))) $ range (minQ, maxQ)
    b@((minQ, minS), (maxQ, maxS)) = bounds ts

propLexerTokenize :: MatchLexerCase -> Property
propLexerTokenize (MatchLexerCase rs i m) = testLexerDfa didTokenizeCorrectly $ generateLexerString rs
    where
    didTokenizeCorrectly lexer = case lexerTokenize lexer m of
        Left _ -> False
        Right [Token s m'] -> m == m' && shouldMatch i && all (\(j, r) -> regexMatches r m == shouldMatch j) (zip [0..] rs)
            where
            shouldMatch = (`elem` ts')
            ts' :: [Int]
            ts' = map read $ splitOn "_OR_" s
        Right _ -> False

testLexerDfa :: (LexerDFA String -> Bool) -> String -> Property
testLexerDfa f = either (const (whenFail (putStrLn "Failed to parse") False)) (property . f) . compileLexer

generateLexerString :: [Regex] -> String
generateLexerString = unlines . zipWith (\i s -> printf "%d %s" i (show s)) [0::Int ..]

data MatchLexerCase = MatchLexerCase [Regex] Int String

instance Show MatchLexerCase where
    show (MatchLexerCase rs i m) = printf "lexer [%s] should match pattern #%d, \"%s\"" rs' i m
        where
        rs' = intercalate ", " $ map (('"':) . (++"\"") . show) rs

instance Arbitrary MatchLexerCase where
    arbitrary = do
        rs <- listOf1 arbitrary
        (i, m) <- (\(i, r) -> do m <- matchedString r; return (i, m)) =<< elements (zip [0..] rs)
        return $ MatchLexerCase rs i m

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
