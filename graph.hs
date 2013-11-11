{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reg (regexpParser, compileEnfaToDfa, DFA(DFA), allCharacterClasses)
import Text.ParserCombinators.Parsec (parse)
import Data.GraphViz hiding (parse)
import Data.GraphViz.Attributes.Complete( Attribute(RankDir, Splines, FontName)
                                        , RankDir(FromLeft), EdgeType(SplineEdges))
import Data.GraphViz.Exception
import Control.Arrow(second)
import Control.Monad (liftM)
import Data.Array (bounds, assocs)
import Data.Ix (range)
import Data.Maybe (isJust, fromJust)
import Data.List (groupBy, sortBy, sort)
import Data.Tuple (swap)
import qualified Data.Set as Set
-- import Control.Exception (handle)
import System.Environment (getArgs)

main :: IO()
main = do
    filePath <- liftM head getArgs
    regex <- getLine
    case parse regexpParser "regex" regex of
        Right enfa -> (graphToDotPng filePath $ dfaToDot $ compileEnfaToDfa enfa) >>= print
        Left err -> print err

dfaToDot :: DFA -> DotGraph Int
dfaToDot dfa = dfaToDotParams (dfaParams dfa) dfa
dfaToDotParams :: Ord cl => GraphvizParams Int () String cl l -> DFA -> DotGraph Int
dfaToDotParams params (DFA ts _ _) = graphElemsToDot params ns es
    where
    ns = map (second $ const ()) $ range $ bounds ts
    es :: [(Int, Int, String)]
    -- filter out transitions that transition to an error state
    -- group all transitions that go from the same states
    es = map consolidateEdges
         $ groupBy (byEdge (==))
         $ sortBy (byEdge compare)
         $ filter (isJust . snd) $ assocs ts
    byEdge f ((from1, _), to1) ((from2, _), to2) = f (from1, to1) (from2, to2)
    consolidateEdges es@(((from, _), to): _) = (from, fromJust to, label)
        where
        symbols = sort $ map (\((_, c), _) -> c) es
        label = case lookup symbols $ map swap allCharacterClasses of
            Just name -> name
            Nothing -> '[' : symbols ++ "]"

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
gStyle = [ GraphAttrs [RankDir FromLeft, Splines SplineEdges, FontName "courier"]
         , NodeAttrs  [textLabel "\\N", shape Circle]
         , EdgeAttrs  [color Black]
                                              ]

graphToDotPng :: (Ord a, PrintDot a, ParseDot a) => FilePath -> DotGraph a -> IO Bool
graphToDotPng fpre g = handle (\(_::GraphvizException) -> return False)
                       $ addExtension (runGraphviz g) Png fpre >> return True
