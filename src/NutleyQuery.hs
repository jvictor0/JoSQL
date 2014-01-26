module NutleyQuery (module NutleyQueryUtils, module NutleyQuery) where

import Data.List
import Data.Serialize

import Name
import Schema
import Types
import HaskellCode
import Utils
import TupleUtils
import Metadata
import Join
import NutleyQueryUtils

import SimpleRecord
import SimpleSubInstance
import InverseImage
import DirectImage
import Shriek
import CoLimit


codeQuery (MaterializeQuery db ss) = codeMaterialize db ss
codeQuery (SectionQuery db ss) = codeSection db ss
codeQuery (InstantiateQuery db) = codeInstantiate db
codeQuery (InstantiateSelectQuery to from ss) = codeInstantiateSelect to from ss

        
codeSection :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSection db ss = case dbToken db of
  DirectImageToken -> codeDirectImageSection db ss
  ShriekToken -> codeShriekSection db ss
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