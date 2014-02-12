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

data SimplicialComplex = SC [(VertID,Name)] [[VertID]] deriving (Generic)
data Schema = Schema SimplicialComplex [(VertID, HaskellType)] deriving (Generic)
type Simplex = [VertID]
type ConnectedSchema = Schema

schemaVertices (Schema (SC verts _) _) = map fst verts
schemaVertexNames (Schema (SC verts _) _) = verts
schemaSimplices (Schema (SC _ simps) _) = simps
schemaTypes (Schema _ ts) = ts

subSchemaSimplices (SubSchema simps _) = simps
showSubSchema (SubSchema simps sch) = "{" ++ (cim "," (\x -> "{" ++ (cim "," id x) ++ "}") nmedsimps) ++ "}"
  where nmedsimps = map (map (fromJust.(flip schemaLookupVertexName sch))) simps
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
    "schema where\n  vertices:\n" ++ 
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

emptySimplicialComplex = SC [] []
emptySchema = Schema emptySimplicialComplex [] 

schemaLookupVertex a sch = fmap fst $ find ((==a).snd) $ schemaVertexNames sch
schemaLookupVertexName i sch = lookup i $ schemaVertexNames sch

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

-- possible TODO: make this only add new simplices
insertSimplices :: [Simplex] -> Schema -> Schema
insertSimplices newSimps (Schema (SC verts simps) vs) = Schema (SC verts (newSimps ++ simps)) vs

schemaCoProduct :: [Schema] -> (Schema,[SchemaMap])
schemaCoProduct schs = (resultSchema,inclusions)
  where n = length schs
        renamed = map (\(i,(Schema (SC verts simps) types))
                       -> Schema 
                          (SC (map (\(v,nmt) -> (n*v+i,nmt)) verts)
                           (map (map (\v -> n*v+i)) simps))
                          (map (\(v,t) -> (n*v+i,t)) types))
                  $ zip [0..] schs
        resultSchema = foldr (\(Schema (SC s1 v1) t1) (Schema (SC s2 v2) t2) -> Schema (SC (s1++s2) (v1++v2)) (t1++t2))
                       (Schema (SC [] []) []) renamed
        inclusions = map (\(i,s@(Schema _ ts)) -> SchemaMap s resultSchema $ map (\(v,t) -> (v,v*n+i,Lit "id")) ts) 
                     $ zip [0..] schs


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
