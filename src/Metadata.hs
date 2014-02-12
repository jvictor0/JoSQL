module Metadata where

import Data.List
import Data.Serialize
import GHC.Generics
import Control.Monad

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
import qualified Data.ByteString as BS

data DBMetadata = SimpleRecordMetadata 
                  {
                    simpleRecordName :: Name,
                    simpleRecordSchema :: Schema,
                    simpleRecordCompressionSchemes   :: [(VertID, HaskellCode)],
                    simpleRecordDecompressionSchemes :: [(VertID, HaskellCode)],
                    simpleRecordHashCode :: BS.ByteString
                  } | 
                  SimpleSubInstanceMetadata
                  {
                    simpleSubInstanceName :: Name,                    
                    simpleSubInstanceSimplex :: Simplex,
                    simpleSubInstanceParamTypes :: [HaskellType],
                    simpleSubInstanceFilterFunc :: HaskellCode,
                    simpleSubInstanceInnerMetadata :: DBMetadata,
                    simpleSubInstanceHashCode :: BS.ByteString
                  } |
                  InverseImageMetadata 
                  {
                    inverseImageName :: Name,
                    inverseImageMap  :: SchemaMap,
                    inverseImageParamTypes :: [HaskellType],                                      
                    inverseImageInnerMetadata :: DBMetadata,
                    inverseImageHashCode :: BS.ByteString
                  } |
                  DirectImageMetadata 
                  {
                    directImageName :: Name,                    
                    directImageMap  :: SchemaMap,
                    directImageInnerMetadata :: DBMetadata,
                    directImageHashCode :: BS.ByteString
                  } |
                  ShriekMetadata 
                  {
                    shriekName :: Name,                    
                    shriekMap  :: SchemaMap,
                    shriekInnerMetadata :: DBMetadata,
                    shriekHashCode :: BS.ByteString
                  } |
                  CoLimitMetadata 
                  {
                    coLimitName :: Name,
                    coLimitInnerMetadatas :: [DBMetadata],
                    coLimitHashCode :: BS.ByteString
                  } |
                  RemoteInstanceMetadata
                  {
                    remoteInstanceName :: Name,
                    remoteInstanceSchema :: Schema,
                    remoteInstanceHashCode :: BS.ByteString
                  }
                  
data MetadataToken = SimpleRecordToken | SimpleSubInstanceToken 
                   | InverseImageToken | DirectImageToken
                   | ShriekToken
                   | CoLimitToken
                   | RemoteInstanceToken
                   deriving (Eq,Show,Ord)

dbToken (SimpleRecordMetadata _ _ _ _ _) = SimpleRecordToken
dbToken (SimpleSubInstanceMetadata _ _ _ _ _ _) = SimpleSubInstanceToken
dbToken (InverseImageMetadata _ _ _ _ _) = InverseImageToken
dbToken (DirectImageMetadata _ _ _ _) = DirectImageToken
dbToken (ShriekMetadata _ _ _ _) = ShriekToken
dbToken (CoLimitMetadata _ _ _) = CoLimitToken
dbToken (RemoteInstanceMetadata _ _ _) = RemoteInstanceToken
                                                         
instance Named DBMetadata where
  name md = case dbToken md of
    SimpleRecordToken -> simpleRecordName md
    SimpleSubInstanceToken -> simpleSubInstanceName md
    InverseImageToken -> inverseImageName md
    DirectImageToken -> directImageName md
    ShriekToken -> shriekName md
    CoLimitToken -> coLimitName md
    RemoteInstanceToken -> remoteInstanceName md

dbSchema :: DBMetadata -> Schema
dbSchema md = case dbToken md of
  SimpleRecordToken -> simpleRecordSchema md
  SimpleSubInstanceToken -> dbSchema $ simpleSubInstanceInnerMetadata md
  InverseImageToken -> schemaMapDomain $ inverseImageMap md
  DirectImageToken -> schemaMapCoDomain $ directImageMap md
  ShriekToken -> schemaMapCoDomain $ shriekMap md
  RemoteInstanceToken -> remoteInstanceSchema md
  CoLimitToken -> case coLimitInnerMetadatas md of 
    [] -> emptySchema
    (a:_) -> dbSchema a
                 
dbHashCode :: DBMetadata -> BS.ByteString
dbHashCode md = case dbToken md of
  SimpleRecordToken -> simpleRecordHashCode md
  SimpleSubInstanceToken -> simpleSubInstanceHashCode md
  InverseImageToken -> inverseImageHashCode md
  DirectImageToken -> directImageHashCode md
  ShriekToken -> shriekHashCode md
  CoLimitToken -> coLimitHashCode md
  RemoteInstanceToken -> remoteInstanceHashCode md
  
instance Eq DBMetadata where
  a == b = (dbHashCode a) == (dbHashCode b)
instance Ord DBMetadata where
  compare a b = compare (dbHashCode a) (dbHashCode b)
  
dbVertexNames :: DBMetadata -> [(VertID,Name)]
dbVertexNames md = schemaVertexNames $ dbSchema md
    
simplexFromNames :: DBMetadata -> [Name] -> Maybe [VertID]
simplexFromNames db simps = let names = map (\(x,y) -> (y,x)) $ dbVertexNames db in 
  mapM (flip lookup names) simps 

simplicesFromNames :: DBMetadata -> [[Name]] -> Maybe [Simplex]
simplicesFromNames db simps = do
  let names = map (\(x,y) -> (y,x)) $ dbVertexNames db 
  simps <- mapM (mapM (flip lookup names)) simps
  guard $ all (containsSimplex (fullSubSchema $ dbSchema db)) simps
  return simps
  
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
    
    