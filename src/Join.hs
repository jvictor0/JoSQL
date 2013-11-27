module Join where

import HaskellCode
import Schema
import TupleUtils
import Types

import Data.List
import Data.Maybe

codeEquiJoin = codeEquiJoinStub

codeEquiJoinStub :: [(Simplex,HaskellCode)] -> HaskellCode
codeEquiJoinStub simps = LComp res tups intersectCode
  where nSimps = zip (map fst simps) [1..]
        nNames = zip (map snd simps) [1..]
        intersects = [(i,s,j,t,intersect s t) | (s,i) <- nSimps, (t,j) <- nSimps, i < j]
        intersectCode = flip map intersects $ \(i,s,j,t,intr) -> 
          ((tupNatN s intr) $$ [Lit $ "tup_" ++ (show i)]) +==+ ((tupNatN t intr) $$ [Lit $ "tup_" ++ (show j)])
        tups = map (\(c,i) -> (Ltp $ "tup_" ++ (show i), c)) nNames
        allVerts = foldr union [] $ map fst simps :: [VertID]
        allTups = map (\i -> (fromJust $ find (\(s,_) -> i`elem`s) nSimps, i)) allVerts
        res = Tpl $ map (\((s,j),i) -> tupNatN s [i] $$ [Lit $ "tup_" ++ (show j)]) allTups
        
