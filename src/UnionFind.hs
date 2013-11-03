module UnionFind where

import qualified Data.Map as Map
import Data.List hiding (union,find)

import Utils

newtype UnionFind a = UF (Map.Map a a)

newUnionFind = UF Map.empty

find :: (Ord a) => UnionFind a -> a -> (UnionFind a,a)
find (UF uf) a = case Map.lookup a uf of
  (Just b) -> let (UF uf',res) = find (UF uf) b in (UF $ Map.insert a res uf',res)
  Nothing  -> (UF uf,a)
  
union :: (Ord a) => UnionFind a -> a -> a -> UnionFind a
union (UF uf) a b = let (UF uf', a') = find (UF uf) a in UF $ Map.insert a' b uf'

findAll (UF uf) = foldr (\a u -> fst $ find u a) (UF uf) $ Map.keys uf

fromSets sets = foldr (\s uf -> foldr (\b u -> union u (head s) b) uf $ tail s) newUnionFind sets

connectedSets :: (Ord a) => [[a]] -> [[a]]
connectedSets sets = let (UF uf) = findAll $ fromSets sets
                     in map (\s -> (snd$head s):(map fst s)) $ groupBy ((==)`on`snd) $ sortBy (compare `on` snd) $ Map.toList uf
