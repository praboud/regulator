module Regulate.Util where


import Data.List (sortBy, groupBy)
import Data.Set (Set)
import qualified Data.Set as Set

sortAndGroupBy :: Ord b => (a -> b) -> [a] -> [[a]]
sortAndGroupBy f = groupBy (\x y -> (f x) == (f y)) . sortBy (\x y -> compare (f x) (f y))

setMapMaybe :: (Ord x, Ord y) => (x -> Maybe y) -> Set x -> Set y
setMapMaybe f = Set.foldr (\x a -> case f x of
    Nothing -> a
    Just y  -> Set.insert y a) Set.empty
