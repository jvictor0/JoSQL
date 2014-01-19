{-# LANGUAGE DeriveGeneric #-}
module Metadata where

import Data.List
import Data.Serialize
import GHC.Generics

import Name
import Schema
import HaskellCode
import NutleyInstance
import Types
import Join
import Verify
import Utils
import Data.Tuple.HT
import TupleUtils

data DBMetadata = SimpleRecordMetadata 
                  {
                    simpleRecordName :: Name,
                    simpleRecordSchema :: Schema,
                    simpleRecordCompressionSchemes   :: [(VertID, HaskellCode)],
                    simpleRecordDecompressionSchemes :: [(VertID, HaskellCode)]
                  } | 
                  SimpleSubInstanceMetadata
                  {
                    simpleSubInstanceName :: Name,                    
                    simpleSubInstanceSimplex :: Simplex,
                    simpleSubInstanceParamTypes :: [HaskellType],
                    simpleSubInstanceFilterFunc :: HaskellCode,
                    simpleSubInstanceInnerMetadata :: DBMetadata
                  } |
                  InverseImageMetadata 
                  {
                    inverseImageName :: Name,
                    inverseImageMap  :: SchemaMap,
                    inverseImageParamTypes :: [HaskellType],                                      
                    inverseImageInnerMetadata :: DBMetadata
                  } |
                  DirectImageMetadata 
                  {
                    directImageName :: Name,                    
                    directImageMap  :: SchemaMap,
                    directImageInnerMetadata :: DBMetadata
                  } |
                  ShriekMetadata 
                  {
                    shriekName :: Name,                    
                    shriekMap  :: SchemaMap,
                    shriekInnerMetadata :: DBMetadata
                  } |
                  CoLimitMetadata 
                  {
                    coLimitName :: Name,
                    coLimitInnerMetadatas :: [DBMetadata]
                  } 
                  deriving (Generic)
                 
instance Serialize DBMetadata 
                           
data MetadataToken = SimpleRecordToken | SimpleSubInstanceToken 
                   | InverseImageToken | DirectImageToken
                   | ShriekToken
                   | CoLimitToken
                   deriving (Eq,Show,Ord)

dbToken (SimpleRecordMetadata _ _ _ _) = SimpleRecordToken
dbToken (SimpleSubInstanceMetadata _ _ _ _ _) = SimpleSubInstanceToken
dbToken (InverseImageMetadata _ _ _ _) = InverseImageToken
dbToken (DirectImageMetadata _ _ _) = DirectImageToken
dbToken (ShriekMetadata _ _ _) = ShriekToken
dbToken (CoLimitMetadata _ _) = CoLimitToken
                                                         
instance Named DBMetadata where
  name md = case dbToken md of
    SimpleRecordToken -> simpleRecordName md
    SimpleSubInstanceToken -> simpleSubInstanceName md
    InverseImageToken -> inverseImageName md
    DirectImageToken -> directImageName md
    ShriekToken -> shriekName md
    CoLimitToken -> coLimitName md

dbSchema :: DBMetadata -> Schema
dbSchema md = case dbToken md of
  SimpleRecordToken -> simpleRecordSchema md
  SimpleSubInstanceToken -> dbSchema $ simpleSubInstanceInnerMetadata md
  InverseImageToken -> schemaMapDomain $ inverseImageMap md
  DirectImageToken -> schemaMapCoDomain $ directImageMap md
  ShriekToken -> schemaMapCoDomain $ shriekMap md
  CoLimitToken -> case coLimitInnerMetadatas md of 
    [] -> emptySchema
    (a:_) -> dbSchema a
                 
dbVertexNames :: DBMetadata -> [(VertID,Name)]
dbVertexNames md = schemaVertexNames $ dbSchema md
    
simplexFromNames :: DBMetadata -> [Name] -> Maybe [VertID]
simplexFromNames db simps = let names = map (\(x,y) -> (y,x)) $ dbVertexNames db in 
  mapM (flip lookup names) simps 

simplicesFromNames :: DBMetadata -> [[Name]] -> Maybe [Simplex]
simplicesFromNames db simps = let names = map (\(x,y) -> (y,x)) $ dbVertexNames db in 
  mapM (mapM (flip lookup names)) simps
-- should go in a different file
instance Verify DBMetadata where
  verifyConditions md = case dbToken md of
    ShriekToken -> let (SchemaMap _ _ f) = shriekMap md in
      [
        (all (isIdentity.thd3) f, "Cannot form shriek map with non-identity pullback maps"),
        (unique $ map snd3 f, "Cannot form shriek with non-injective schema map")
      ]
    DirectImageToken -> let (SchemaMap _ trg f) = directImageMap md in
      [ -- TODO: check the types of the tupnatinverse
        ((length (schemaVertices trg)) == (length $ nub $ (map snd3 f)), "Cannot form direct image with map not surjective on vertices"),
        (all ((/=Nothing).tupNatInverse) $ map thd3 f, "Cannot form direct image with pullback maps not projections")
      ]
    _ -> []
    
    