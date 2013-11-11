{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reg (regexpParser, compileEnfaToDfa, DFA(DFA), allCharacterClasses, LexerDFA(LexerDFA), compileLexer, lexerParser)
import Text.ParserCombinators.Parsec (parse)
import Data.GraphViz hiding (parse)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Exception
import Control.Arrow(second)
import Data.Array (bounds, assocs)
import Data.Ix (range)
import Data.Maybe (isJust, fromJust)
import Data.List (groupBy, sortBy, sort)
import Data.List.Ordered (nub)
import Data.Char (showLitChar)
import qualified Data.Set as Set
-- import Control.Exception (handle)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    (dfa, outPath) <- case args of
        [lexerPath, outPath] -> getDfaFromLexer lexerPath >>= passBack outPath
        [outPath] -> getDfaFromRegex >>= passBack outPath
        _ -> fail "Unknown arguments specified"
    case dfa of
        Just dfa' -> (graphToDotPng outPath $ dfaToDot dfa') >>= print
        Nothing -> fail "Could not parse expression into DFA"
    where
    passBack outPath dfa = return (dfa, outPath)

getDfaFromLexer :: FilePath -> IO(Maybe DFA)
getDfaFromLexer lexFilePath = do
    lexfile <- readFile lexFilePath
    case parse lexerParser "lexer" lexfile of
        Right lexer -> return $ Just dfa
            where (LexerDFA dfa _) = compileLexer lexer
        Left _ -> return Nothing

getDfaFromRegex :: IO(Maybe DFA)
getDfaFromRegex = do
    regex <- getLine
    case parse regexpParser "regex" regex of
        Right enfa -> return $ Just $ compileEnfaToDfa enfa
        Left _ -> return Nothing


dfaToDot :: DFA -> DotGraph Int
dfaToDot dfa = dfaToDotParams (dfaParams dfa) dfa
dfaToDotParams :: Ord cl => GraphvizParams Int () String cl l -> DFA -> DotGraph Int
dfaToDotParams params (DFA ts _ _) = graphElemsToDot params ns es
    where
    ns = nub $ map (second $ const ()) $ range $ bounds ts
    es :: [(Int, Int, String)]
    -- filter out transitions that transition to an error state
    -- group all transitions that go from the same states
    es = map consolidateEdges
         $ groupBy (byEdge (==))
         $ sortBy (byEdge compare)
         $ filter (isJust . snd) $ assocs ts
    byEdge f ((from1, _), to1) ((from2, _), to2) = f (from1, to1) (from2, to2)
    consolidateEdges edges@(((from, _), to): _) = (from, fromJust to, label)
        where
        symbols = sort $ map (\((_, c), _) -> c) edges
        label = '[' : (findClosestClass symbols) ++ "]"
    classes = ("", Set.empty) : map (\(name, syms) -> (name, Set.fromList syms)) allCharacterClasses
    findClosestClass symbols = clsName
                               ++ (foldr showLitChar "" $ Set.toList (Set.difference symbolSet clsSet))
                               ++ (foldr (\s a -> ('^':) $ showLitChar s a) "" $ Set.toList (Set.difference clsSet symbolSet))
        where
        (clsName, clsSet, _) = match
        match = head $ sortBy (\(_, _, s1) (_, _, s2) -> compare s1 s2)
            $ map (\(name, set) -> (name, set, Set.size $ diff symbolSet set)) classes
        symbolSet = Set.fromList symbols
        diff a b = Set.difference (Set.union a b) (Set.intersection a b)

dfaParams :: DFA -> GraphvizParams Int () String () ()
dfaParams (DFA _ q as) = defaultParams
    { globalAttributes = gStyle
    , fmtNode = dfaNodeFormat
    , fmtEdge = (\(_, _, el) -> [toLabel el])
    }
    where
    dfaNodeFormat :: (Int, ()) -> Attributes
    dfaNodeFormat (n, _) = [shape s, color c]
        where
        s = if Set.member n as then DoubleCircle else Circle
        c = if n == q then Red else Black

gStyle :: [GlobalAttributes]
gStyle = [ gAttrs
         , NodeAttrs  [textLabel "\\N", shape Circle]
         , EdgeAttrs  [color Black]
         ]
    where
    gAttrs = GraphAttrs
        [ RankDir FromLeft
        , Splines SplineEdges
        , FontName "terminus"
        , Size $ GSize { width = 100, height = Nothing, desiredSize = True }
        , Concentrate True
        ]

graphToDotPng :: (Ord a, PrintDot a, ParseDot a) => FilePath -> DotGraph a -> IO Bool
graphToDotPng fpre g = handle (\(_::GraphvizException) -> return False)
                       $ addExtension (runGraphviz g) Pdf fpre >> return True
