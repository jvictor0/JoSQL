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

data DBMetadata = SimpleRecordMetadata 
                  {
                    simpleRecordName :: Name,
                    simpleRecordSchema :: Schema,
                    simpleRecordVertexNames :: [(VertID,Name)],
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
                    inverseImageVertexNames :: [(VertID,Name)],                     
                    inverseImageParamTypes :: [HaskellType],                                      
                    inverseImageInnerMetadata :: DBMetadata
                  } |
                  DirectImageMetadata 
                  {
                    directImageName :: Name,                    
                    directImageVertexNames :: [(VertID,Name)],                     
                    directImageMap  :: SchemaMap,
                    directImageInnerMetadata :: DBMetadata
                  } |
                  ShriekMetadata 
                  {
                    shriekName :: Name,                    
                    shriekMap  :: SchemaMap,
                    shriekVertexNames :: [(VertID,Name)],                     
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

dbToken (SimpleRecordMetadata _ _ _ _ _) = SimpleRecordToken
dbToken (SimpleSubInstanceMetadata _ _ _ _ _) = SimpleSubInstanceToken
dbToken (InverseImageMetadata _ _ _ _ _) = InverseImageToken
dbToken (DirectImageMetadata _ _ _ _) = DirectImageToken
dbToken (ShriekMetadata _ _ _ _) = ShriekToken
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
  DirectImageToken -> schemaMapDomain $ directImageMap md
  ShriekToken -> schemaMapCoDomain $ shriekMap md
  CoLimitToken -> case coLimitInnerMetadatas md of 
    [] -> emptySchema
    (a:_) -> dbSchema a
                 
dbVertexNames :: DBMetadata -> [(VertID,Name)]
dbVertexNames md = case dbToken md of
  SimpleRecordToken -> simpleRecordVertexNames md
  SimpleSubInstanceToken -> dbVertexNames $ simpleSubInstanceInnerMetadata md
  InverseImageToken -> inverseImageVertexNames md
  DirectImageToken -> directImageVertexNames md
  ShriekToken -> shriekVertexNames md
  CoLimitToken -> case coLimitInnerMetadatas md of 
    [] -> []
    (a:_) -> dbVertexNames a
    
simplexFromNames :: DBMetadata -> [Name] -> Maybe [VertID]
simplexFromNames db simps = let names = map (\(x,y) -> (y,x)) $ dbVertexNames db in 
  mapM (flip lookup names) simps
 
simplicesFromNames :: DBMetadata -> [[Name]] -> Maybe [Simplex]
simplicesFromNames db simps = let names = map (\(x,y) -> (y,x)) $ dbVertexNames db in 
  mapM (mapM (flip lookup names)) simps