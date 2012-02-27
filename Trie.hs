module Trie 
    ( Trie
    , empty
    , insert
    , find
    , complete
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

data Trie a = Trie (Map a (Trie a)) Bool

empty    :: Ord a => Trie a
insert   :: Ord a => [a] -> Trie a -> Trie a
find     :: Ord a => [a] -> Trie a -> Bool
complete :: Ord a => [a] -> Trie a -> [[a]]

empty = Trie Map.empty False

insert []      (Trie m _) = Trie m True
insert (c : w) (Trie m b) =
    case Map.lookup c m of
        Nothing  -> insert (c : w) $ Trie (Map.insert c empty m) b
        Just tr' -> Trie (Map.insert c (insert w tr') m) b

find []      (Trie _ b) = b
find (c : w) (Trie m _) = maybe False (find w) $ Map.lookup c m

complete []      (Trie m b) =
    [[] | b] ++ concat [map (c :) (complete [] tr) | (c, tr) <- Map.toList m]    
complete (c : w) (Trie m _) =
    maybe [] (map (c :) . complete w) $ Map.lookup c m
