module Regulate.Util where


import Data.List (sortBy, groupBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)

sortAndGroupOn :: Ord b => (a -> b) -> [a] -> [[a]]
sortAndGroupOn f = groupBy ((==) `on` f ) . sortBy (compare `on` f)

setMapMaybe :: (Ord x, Ord y) => (x -> Maybe y) -> Set x -> Set y
setMapMaybe f = Set.foldr (\x a -> case f x of
    Nothing -> a
    Just y  -> Set.insert y a) Set.empty
