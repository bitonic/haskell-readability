module Trie 
    ( empty
    , insert
    , find
    , complete
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

data Trie a = Trie (Map a (Trie a)) Bool
    deriving (Show)

empty :: Ord a => Trie a
empty = Trie Map.empty False

add :: Ord a => a -> Trie a -> Trie a
add char (Trie m b) = Trie (Map.insert char empty m) b

insert :: Ord a => [a] -> Trie a -> Trie a
insert []      (Trie m _) = Trie m True
insert (c : w) tr@(Trie m b) =
    case Map.lookup c m of
        Nothing  -> insert (c : w) $ add c tr
        Just tr' -> Trie (Map.insert c (insert w tr') m) b

find :: Ord a => [a] -> Trie a -> Bool
find []      (Trie _ b) = b
find (c : w) (Trie m _) = maybe False (find w) $ Map.lookup c m

complete :: Ord a => [a] -> Trie a -> [[a]]
complete []      (Trie m b) =
    let h = if b then [[]] else []
    in  h ++ concat [map (c :) (complete [] tr) | (c, tr) <- Map.toList m]
complete (c : w) (Trie m _) =
    maybe [] (map (c :) . complete w) $ Map.lookup c m
