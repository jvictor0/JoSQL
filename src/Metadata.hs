{-# LANGUAGE ExistentialQuantification #-}
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

data NutleyQuery = forall dbmd . (DBMetadata dbmd) => MaterializeQuery dbmd SubSchema 
                 | forall dbmd . (DBMetadata dbmd) => SectionQuery dbmd SubSchema 
                 | forall dbmd . (InstantiateableDBMetadata dbmd) => InstantiateQuery dbmd

instance Named NutleyQuery where
  name (MaterializeQuery metadata ss) = materializeFName metadata ss
  name (SectionQuery metadata ss) = sectionFName metadata ss
  name (InstantiateQuery metadata) = instantiateFName metadata


codeQuery (MaterializeQuery db ss) = codeMaterialize db ss
codeQuery (SectionQuery db ss) = codeSection db ss
codeQuery (InstantiateQuery db) = codeInstantiate db

materializeFName metadata subschema = "materialize_" ++ (name metadata) ++ "_" ++ (name subschema)
sectionFName metadata subschema = "section_" ++ (name metadata) ++ "_" ++ (name subschema)
instantiateFName metadata = "instantiate_" ++ (name metadata)

materializeType metadata ss@(SubSchema simps _) = funType
  where inTupType = TupType $ map (tc_List.TupType.(univ (dbSchema metadata))) simps
        funType = FunType [instanceType metadata] $ tc_IO $ inTupType
        verts = nub $ concat simps

sectionType :: (DBMetadata dbmd) => dbmd -> SubSchema -> HaskellType
sectionType metadata (SubSchema simps _) = FunType [instanceType metadata] $ tc_IO $ outTupType         
  where outTupType = tc_List $ TupType $ map (typeLookup $ dbSchema metadata) allVerts
        allVerts = sort $ foldr union [] $ simps :: [VertID]

class (Named dbmd, Serialize dbmd) => DBMetadata dbmd where
  dbSchema :: dbmd -> Schema
  
  instanceType :: dbmd -> HaskellType
  
  codeMaterialize :: dbmd -> SubSchema -> ([NutleyQuery],HaskellFunction)
  codeMaterialize metadata ss@(SubSchema simps sch) = 
    (map (\s -> SectionQuery metadata (SubSchema [s] sch)) simps,
     Fun (materializeFName metadata ss) (materializeType metadata ss)
     $ Lam (Ltp "instID") $ Do $ sects ++ [do_return result]
     )
    where sects = map (\(i,s) -> (Ltp $ "sec_" ++ (show i), 
                                  c_1 (sectionFName metadata (SubSchema [s] sch)) (Lit "instID")))
                  $ zip [1..] simps
          result = Tpl $ map (\(i,_) -> Lit $ "sec_" ++ (show i)) $ zip [1..] simps
  
  codeSection :: dbmd -> SubSchema -> ([NutleyQuery],HaskellFunction)
  codeSection metadata ss@(SubSchema [] _) = 
    ([],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ Lam (Ltp "instID") $ c_return $ Lst [])
  codeSection metadata ss@(SubSchema [x] _) =
    ([MaterializeQuery metadata ss],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ Lam (Ltp "instID") $ materializeFun)
    where materializeFun = (Lit $ materializeFName metadata ss) $$ [Lit "instID"]
  codeSection metadata ss@(SubSchema simps sch) = 
    ([MaterializeQuery metadata ss],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ Lam (Ltp "instID") $ Do [materializeFun, (USp, c_return joinCode)])
    where materializeFun = (Tup $ map (\(_,i) -> Ltp $ "column_" ++ (show i)) $ zip simps [1..], 
                            (Lit $ materializeFName metadata ss) $$ [Lit "instID"])
          material = Lit "materializedCols"
          joinCode = codeEquiJoin $ map (\(s,i) -> (s,Lit $ "column_" ++ (show i))) $ zip simps [1..]


class (DBMetadata dbmd) => InstantiateableDBMetadata dbmd where
  codeInstantiate :: dbmd -> ([NutleyQuery],HaskellFunction)

t_SimpleRecord = t_ "SimpleRecord"
tc_SimpleSubInstance = tc_2 "SimpleSubInstance"
tc_DirectImage = tc_2 "DirectImage"
tc_InverseImage = tc_2 "InverseImage"
tc_Shriek = tc_1 "Shriek"



