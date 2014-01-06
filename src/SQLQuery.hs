{-# LANGUAGE DeriveGeneric #-}
module SQLQuery where

import Data.Tuple.HT
import GHC.Generics

import HaskellCode
import Schema
import Utils
import Types
import Metadata

import SimpleRecord
import SimpleSubInstance
import Shriek
import DirectImage
import InverseImage
import CoLimit


data Hand = LeftHand | RightHand deriving (Eq,Show,Ord,Read,Generic)

data On t = On [(t,VertID,Name)] HaskellCode

onFunction (On args c) = Lam (Tup $ map (Ltp . thd3) args) c

data Join = Join DBMetadata DBMetadata (On Hand)

joinToJoSQL :: Join -> DBMetadata
joinToJoSQL (Join l r o@(On vs f)) = subInstance onSimp (onFunction o) prefilter
  where coProd = coProduct [l,r]
        [l_inc,r_inc]  = map shriekMap $ coLimitInnerMetadatas coProd
        onVert (LeftHand,v,_) = mapApplyVertex l_inc v
        onVert (RightHand,v,_) = mapApplyVertex r_inc v
        onSimp = map onVert vs
        dirImMap = SchemaMap (dbSchema coProd) (insertSimplices [onSimp] $ dbSchema coProd)
                   $ map (\v -> (v,v,Lit "id")) $ schemaVertices $ dbSchema coProd
        prefilter = directImage dirImMap coProd
        
        