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
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.List (sort, partition, minimumBy)
import Data.Function (on)
import Data.Char (showLitChar)
import Data.Set (Set)
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
    ns = map (\q -> (q, ())) $ range (qLo, qHi)
    ((qLo, _), (qHi, _)) = bounds ts
    es :: [(Int, Int, String)]
    es = groupEdges $ mapMaybe (\((from, c), to) -> to >> Just (from, fromJust to, c)) $ assocs ts

-- filter out transitions that transition to an error state
-- group all transitions that go from the same states
groupEdges :: (Ord x, Ord y) => [(x, y, Char)] -> [(x, y, String)]
groupEdges = map consolidateEdges . sortAndGroupOn (\(x, y, _) -> (x, y))
    where
    consolidateEdges [] = undefined
    consolidateEdges edges@((from, to, _): _) = (from, to, label)
        where
        symbols = sort $ map (\(_, _, c) -> c) edges
        label = '[' : findClosestClass symbols ++ "]"

enfaToDot :: ENFA -> DotGraph Int
enfaToDot enfa@(ENFA ts _ _)  = graphElemsToDot (enfaParams enfa) ns es
    where
    epsilonStr = "\x03b5"
    third f (a, b, c) = (a, b, f c)
    ns = map (\q -> (q, ())) $ Set.toList $ enfaStateSet enfa
    es = groupEdges (map (third fromJust) nonEpsilon) ++ map (third $ const epsilonStr) epsilon
    -- transitions through the epsilon and non epsilon character respectively
    (nonEpsilon, epsilon) = partition (\(_, _, c) -> isJust c) res
    -- list of all transitions, in the form (from, to, Maybe by)
    -- state / node the transition comes from, state it goes to, and the charater it goes by
    res :: [(Int, Int, Maybe Char)]
    res = Map.foldrWithKey (\from ts' ac -> Map.foldrWithKey (\c tos ac' -> Set.foldr (\to ac'' -> (from, to, c) : ac'') ac' tos) ac ts') [] ts

classes :: [(String, Set Char)]
classes = ("", Set.empty) : map (second Set.fromList) allCharacterClasses

-- tries to find the character class which has the most in common with a set of characters,
-- if one exists, otherwise, simply display the character in the set
-- after finding the closest match, display the difference between the set and the character class
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
dfaParams  = graphParams (\(DFA _ q as) -> nodeAttrs q as . fst)

enfaParams :: ENFA -> GraphvizParams Int () String () ()
enfaParams = graphParams (\(ENFA _ q as) -> nodeAttrs q as . fst)

-- styles node / state based on whether the state is the starting node,
-- and / or an accepting node.
nodeAttrs :: Ord a => a -> Set a -> a -> Attributes
nodeAttrs q as n = [shape s, color c]
    where
    s = if Set.member n as then DoubleCircle else Circle
    c = if n == q then Red else Black

-- generic styling, independant of enfa / dfa specific logic
graphParams :: (a -> (Int, ()) -> Attributes) -> a -> GraphvizParams Int () String () ()
graphParams nodeFormat graph = defaultParams
    { globalAttributes = gStyle
    , fmtNode = nodeFormat graph
    , fmtEdge = \(_, _, el) -> [toLabel el]
    }
    where
    gStyle =
        [ gAttrs
        , NodeAttrs  [textLabel "\\N", shape Circle]
        , EdgeAttrs  [color Black]
        ]
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
