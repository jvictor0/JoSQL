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
  DirectImageToken -> schemaMapDomain $ directImageMap md
  ShriekToken -> schemaMapCoDomain $ shriekMap md
  CoLimitToken -> case coLimitInnerMetadatas md of 
    [] -> emptySchema
    (a:_) -> dbSchema a
                 
