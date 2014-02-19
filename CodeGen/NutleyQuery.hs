module CodeGen.NutleyQuery (module CodeGen.NutleyQueryUtils, module CodeGen.NutleyQuery) where

import Data.List
import Data.Serialize

import Data.Name
import Data.Schema
import Data.Types
import CodeGen.HaskellCode
import Utils.Utils
import CodeGen.TupleUtils
import CodeGen.Metadata.Metadata
import CodeGen.Join
import CodeGen.NutleyQueryUtils

import CodeGen.Metadata.SimpleRecord
import CodeGen.Metadata.SimpleSubInstance
import CodeGen.Metadata.InverseImage
import CodeGen.Metadata.DirectImage
import CodeGen.Metadata.Shriek
import CodeGen.Metadata.CoLimit
--import RemoteInstance

codeQuery (MaterializeQuery db ss) = codeMaterialize db ss
codeQuery (SectionQuery db ss) = codeSection db ss
codeQuery (InstantiateQuery db) = codeInstantiate db
codeQuery (InstantiateSelectQuery to from ss) = codeInstantiateSelect to from ss

        
codeSection :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSection db ss = case dbToken db of
  DirectImageToken -> codeDirectImageSection db ss
  ShriekToken -> codeShriekSection db ss
--  RemoteInstanceToken -> codeRemoteInstanceSection db ss
  _           -> codeSectionDefault db ss

codeMaterialize :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeMaterialize db ss = case dbToken db of
  SimpleRecordToken -> codeSimpleRecordMaterialize db ss
  SimpleSubInstanceToken -> codeSimpleSubInstanceMaterialize db ss
  InverseImageToken -> codeInverseImageMaterialize db ss
  ShriekToken -> codeShriekMaterialize db ss
  CoLimitToken -> codeCoLimitMaterialize db ss
  _   -> codeMaterializeDefault db ss
  
codeInstantiate :: DBMetadata -> ([(Name,NutleyQuery)],HaskellFunction)
codeInstantiate db = case dbToken db of
    SimpleRecordToken -> ([],codeSimpleRecordInstantiate db)
    _  -> error "Cannot Instantiate from Non Simple Record Database Metadata"


codeInstantiateSelect = codeInstantiateSelectDefault