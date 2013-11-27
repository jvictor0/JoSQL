{-# LANGUAGE DeriveGeneric #-}
module Schema where

import Utils
import Types
import Name
import HaskellCode
import qualified UnionFind as UF

import Data.Tuple.HT
import Data.Maybe
import Data.List 
import Control.Monad
import qualified Data.Map as Map
import Data.Serialize
import qualified Data.ByteString as BS
import GHC.Generics

data SimplicialComplex = SC [VertID] [[VertID]] deriving (Generic)
data Schema = Schema SimplicialComplex [(VertID, HaskellType)] deriving (Generic)
type Simplex = [VertID]
type ConnectedSchema = Schema

schemaVertices (Schema (SC verts _) _) = verts
schemaSimplices (Schema (SC _ simps) _) = simps
schemaTypes (Schema _ ts) = ts

subSchemaSimplices (SubSchema simps _) = simps
subSchemaVertices ss = nub $ concat $ subSchemaSimplices ss

data SubSchema = SubSchema [Simplex] Schema deriving (Generic)
            
face :: Int -> Simplex -> Simplex
face = deleteAt

instance Show SimplicialComplex where
  show (SC verts simps) = 
    "schema " ++ " where\n  vetices:\n" ++ 
    (concatMap (\v -> "    " ++ (show v)  ++ "\n") verts) ++ "\n  simplices:\n" ++ 
    (concatMap (\vs -> "    (" ++ (init $ tail $ show vs) ++ ")\n") simps)
    
instance Show Schema where
  show (Schema (SC _ simps) types) = 
    "schema " ++ " where\n  vertices:\n" ++ 
    (concatMap (\(v,t) -> "    " ++ (show v) ++ " :: " ++ (show t) ++ "\n") types) ++ "\n  simplices:\n" ++
    (concatMap (\vs -> "    (" ++ (init $ tail $ show vs) ++ ")\n") simps)

instance Serialize SimplicialComplex 
instance Serialize Schema
instance Serialize SubSchema
     
instance Show SubSchema where
  show (SubSchema simps _) =
    "subschema containing\n" ++ (concatMap (\s -> "  " ++ (show s) ++ "\n") simps)

instance Named SubSchema where
  name (SubSchema simps _) = concatMap (\s -> "s" ++ (name s) ++ "_") simps

typeLookup :: Schema -> VertID -> HaskellType
typeLookup (Schema _ ts) x = case lookup x ts of
  Nothing -> error "typeLookup: vertex not in Schema"
  (Just t) -> t

univ :: Schema -> Simplex -> [HaskellType]
univ sch x = map (typeLookup sch) $ x

connectedComponants :: SubSchema -> [SubSchema]
connectedComponants (SubSchema simps schema) = map (\(i,simps) -> SubSchema simps schema)
                                                 $ zip [1..] (map (map fst) simpGroups)
  where uf0 = UF.fromSets $ simps
        simpGroups = sortGroupBy snd $ snd
                     $ mapAccumR (\uf s -> let (uf',rep) = UF.find uf $ head $ s in (uf',(s,rep))) uf0 simps
                     
fullSubSchema sch = SubSchema (schemaSimplices sch) sch

containsSimplex :: SubSchema -> Simplex -> Bool
containsSimplex (SubSchema simps _) simp = any (simp`subset`) simps

containsSubSchema :: SubSchema -> SubSchema -> Bool
containsSubSchema ss (SubSchema simps _) = all (containsSimplex ss) simps
        
vertexSpan :: Schema -> [VertID] -> SubSchema
vertexSpan schema verts = SubSchema (map (intersect verts) $ schemaSimplices schema) schema






data SchemaMap = SchemaMap Schema Schema [(VertID,VertID,HaskellCode)] deriving (Generic)

instance Serialize SchemaMap


schemaMapCoDomain (SchemaMap _ trg _) = trg
schemaMapDomain (SchemaMap src _ _) = src

mapVertexFunc :: SchemaMap -> VertID -> HaskellCode
mapVertexFunc (SchemaMap _ _ f) v = case find (\(x,_,_) -> x==v) f of
  (Just (_,_,g)) -> g
  Nothing -> error $ "mapVertexFunc called on " ++ (show v) ++ " with map:\n" ++ (show f) 

mapApplyVertex :: SchemaMap -> VertID -> VertID
mapApplyVertex (SchemaMap _ _ f) v = snd3 $ fromJust $ find (\(x,_,_) -> x==v) f

simplexImage :: SchemaMap -> Simplex -> Simplex
simplexImage (SchemaMap _ _ f) simp = map (\v -> snd3 $ fromJust $ find (\(x,_,_) -> x==v) f) simp

simplexIncluded :: SchemaMap -> Simplex -> Maybe Simplex
simplexIncluded (SchemaMap src _ f) vs = mapM (\v -> fmap fst3 $ find ((==v).snd3) f) vs 

simplexPreimage :: SchemaMap -> Simplex -> SubSchema
simplexPreimage (SchemaMap src _ f) vs = vertexSpan src (map fst3 $ filter (\(_,u,_) -> u `elem` vs) f) 

schemaImage :: SubSchema -> SchemaMap -> SubSchema
schemaImage (SubSchema simps _) f@(SchemaMap _ trg _) = SubSchema (map (simplexImage f) simps) trg

schemaPreimage :: SubSchema -> SchemaMap -> SubSchema
schemaPreimage (SubSchema simps _) f@(SchemaMap src _ _) = SubSchema (concatMap (subSchemaSimplices.(simplexPreimage f)) simps) src

schemaFullImage f = schemaImage (fullSubSchema $ schemaMapDomain f) f
schemaFullPreimage f = schemaPreimage (fullSubSchema $ schemaMapCoDomain f) f
