{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Regulate (parseRegex, compileRegex)
import Regulate.Types
import Regulate.Util (sortAndGroupOn)
import Regulate.Enfa (enfaStateSet)
import Regulate.Lexer (compileLexer)
import Regulate.Parse (allCharacterClasses)

import Data.GraphViz hiding (parse)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Exception

import Control.Monad (liftM)
import Control.Arrow(second)

import Data.Array (bounds, assocs)
import Data.Ix (range)
import Data.Maybe (isJust, fromJust)
import Data.List (sort, partition, minimumBy)
import Data.List.Ordered (nub)
import Data.Function (on)
import Data.Char (showLitChar)
import qualified Data.Set as Set
import qualified Data.Map as Map

import System.Environment (getArgs)

main :: IO ()
main = do
    [mode, outPath] <- getArgs
    genGraph <- case mode of
        "lexerDfa" -> return (liftM (dfaToDot . getLexerDfa) . compileLexer)
        "regexEnfa" -> return (liftM enfaToDot . parseRegex)
        "regexDfa" -> return (liftM dfaToDot . compileRegex)
        _ -> fail "Unknown argument specified"
    dot <- liftM genGraph getContents
    case dot of
        Right dot' -> graphToDotPng outPath dot' >>= print
        Left err -> putStrLn err
    where
    getLexerDfa (LexerDFA dfa _) = dfa

dfaToDot :: DFA -> DotGraph Int
dfaToDot dfa@(DFA ts _ _) = graphElemsToDot (dfaParams dfa) ns es
    where
    ns = nub $ map (second $ const ()) $ range $ bounds ts
    es :: [(Int, Int, String)]
    es = groupEdges $ map (\((from, c), to) -> (from, fromJust to, c)) $ filter (isJust . snd) $ assocs ts

-- filter out transitions that transition to an error state
-- group all transitions that go from the same states
groupEdges :: (Ord x, Ord y) => [(x, y, Char)] -> [(x, y, String)]
groupEdges = map consolidateEdges . sortAndGroupOn (\(x, y, _) -> (x, y))
    where
    consolidateEdges edges@((from, to, _): _) = (from, to, label)
        where
        symbols = sort $ map (\(_, _, c) -> c) edges
        label = '[' : findClosestClass symbols ++ "]"

enfaToDot :: ENFA -> DotGraph Int
enfaToDot enfa@(ENFA ts _ _)  = graphElemsToDot (enfaParams enfa) ns es
    where
    ns = map (\q -> (q, ())) $ Set.toList $ enfaStateSet enfa
    es = groupEdges (map (\(from, to, c) -> (from, to, fromJust c)) nonEpsilon) ++ map (\(from, to, _) -> (from, to, "None")) epsilon
    (nonEpsilon, epsilon) = partition (\(_, _, c) -> isJust c) res
    res :: [(Int, Int, Maybe Char)]
    res = Map.foldrWithKey (\from ts' ac -> Map.foldrWithKey (\c tos ac' -> Set.foldr (\to ac'' -> (from, to, c) : ac'') ac' tos) ac ts') [] ts

classes :: [(String, Set.Set Char)]
classes = ("", Set.empty) : map (second Set.fromList) allCharacterClasses

findClosestClass :: String -> String
findClosestClass symbols = clsName
                           ++ foldr showLitChar "" (Set.toList (Set.difference symbolSet clsSet))
                           ++ foldr (\s a -> ('^':) $ showLitChar s a) "" (Set.toList (Set.difference clsSet symbolSet))
    where
    (clsName, clsSet, _) = match
    match = minimumBy (compare `on` (\(_, _, x) -> x))
        $ map (\(name, set) -> (name, set, Set.size $ diff symbolSet set)) classes
    symbolSet = Set.fromList symbols
    diff a b = Set.difference (a `Set.union` b) (Set.intersection a b)

dfaParams :: DFA -> GraphvizParams Int () String () ()
dfaParams (DFA _ q as) = defaultParams
    { globalAttributes = gStyle
    , fmtNode = dfaNodeFormat
    , fmtEdge = \(_, _, el) -> [toLabel el]
    }
    where
    dfaNodeFormat :: (Int, ()) -> Attributes
    dfaNodeFormat (n, _) = [shape s, color c]
        where
        s = if Set.member n as then DoubleCircle else Circle
        c = if n == q then Red else Black

enfaParams :: ENFA -> GraphvizParams Int () String () ()
enfaParams (ENFA _ q as) = defaultParams
    { globalAttributes = gStyle
    , fmtNode = enfaNodeFormat
    , fmtEdge = \(_, _, el) -> [toLabel el]
    }
    where
    enfaNodeFormat :: (Int, ()) -> Attributes
    enfaNodeFormat (n, _) = [shape s, color c]
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
        , Size GSize { width = 100, height = Nothing, desiredSize = True }
        , Concentrate True
        ]

graphToDotPng :: (Ord a, PrintDot a, ParseDot a) => FilePath -> DotGraph a -> IO Bool
graphToDotPng fpre g = handle (\(_::GraphvizException) -> return False)
                       $ addExtension (runGraphviz g) Pdf fpre >> return True
