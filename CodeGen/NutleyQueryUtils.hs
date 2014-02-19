module CodeGen.NutleyQueryUtils where

import Data.List

import Data.Name
import Data.Schema
import Data.Types
import CodeGen.HaskellCode
import CodeGen.TupleUtils
import Utils.Utils
import CodeGen.Metadata.Metadata
import CodeGen.Join

data NutleyQuery = MaterializeQuery DBMetadata SubSchema 
                 | SectionQuery DBMetadata SubSchema 
                 | InstantiateQuery DBMetadata
                 | InstantiateSelectQuery DBMetadata DBMetadata SubSchema

instance Named NutleyQuery where
  name (MaterializeQuery metadata ss) = materializeFName metadata ss
  name (SectionQuery metadata ss) = sectionFName metadata ss
  name (InstantiateQuery metadata) = instantiateFName metadata
  name (InstantiateSelectQuery to from ss) = instantiateSelectFName to from ss

materializeFName metadata subschema = "materialize_" ++ (name metadata) ++ "_" ++ (name subschema)
sectionFName metadata subschema = "section_" ++ (name metadata) ++ "_" ++ (name subschema)
instantiateFName metadata = "instantiate_" ++ (name metadata)
instantiateSelectFName to from ss = "instantiate_select_" ++ (name to) ++ "_from_" ++ (name from) ++ "_" ++ (name ss)

materializeType metadata ss@(SubSchema simps _) = funType
  where inTupType = TupType $ map (tc_List.TupType.(univ (dbSchema metadata))) simps
        funType = FunType [t_NutleyInstance] $ tc_ErrorT t_IO $ inTupType
        verts = nub $ concat simps

sectionType :: DBMetadata -> SubSchema -> HaskellType
sectionType metadata (SubSchema simps _) = FunType [t_NutleyInstance] $ tc_ErrorT t_IO $ outTupType         
  where outTupType = tc_List $ TupType $ map (typeLookup $ dbSchema metadata) allVerts
        allVerts = foldr union [] $ simps :: [VertID]


-- TODO: make this work for functors
nontrivialMaterialization :: SubSchema -> DBMetadata -> Bool
nontrivialMaterialization ss md = case dbToken md of
  SimpleRecordToken -> any (`subset`(map fst $ simpleRecordCompressionSchemes md)) $ subSchemaSimplices ss
  SimpleSubInstanceToken -> nontrivialMaterialization ss $ simpleSubInstanceInnerMetadata md
  CoLimitToken -> any (nontrivialMaterialization ss) $ coLimitInnerMetadatas md
  _ -> True

codeMaterializeDefault :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeMaterializeDefault metadata ss@(SubSchema simps sch) = 
  (map (\(i,s) -> ("IMP" ++ (show i),SectionQuery metadata (SubSchema [s] sch))) $ zip [1..] simps,
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Ltp "instID") $ Do $ sects ++ [do_return result]
  )
  where sects = map (\(i,s) -> (Ltp $ "sec_" ++ (show i), 
                                c_1 ("IMP" ++ (show i) ++ "." ++ (sectionFName metadata (SubSchema [s] sch))) (Lit "instID")))
                $ zip [1..] simps
        result = Tpl $ map (\(i,_) -> Lit $ "sec_" ++ (show i)) $ zip [1..] simps

codeSectionDefault :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSectionDefault metadata ss@(SubSchema [] _) = 
  ([],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ Lam (Ltp "instID") $ c_return $ Lst [])
codeSectionDefault metadata ss@(SubSchema [x] _) =
  ([("I",MaterializeQuery metadata ss)],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ Lam (Ltp "instID") $ materializeFun)
  where materializeFun = (Lit $ "I." ++ (materializeFName metadata ss)) $$ [Lit "instID"]
codeSectionDefault metadata ss@(SubSchema simps sch) = 
  ([("I",MaterializeQuery metadata ss)],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ Lam (Ltp "instID") $ Do [materializeFun, (USp, c_return joinCode)])
  where materializeFun = (Tup $ map (\(_,i) -> Ltp $ "column_" ++ (show i)) $ zip simps [1..], 
                          (Lit $ "I." ++ ( materializeFName metadata ss)) $$ [Lit "instID"])
        material = Lit "materializedCols"
        joinCode = codeEquiJoin $ map (\(s,i) -> (s,Lit $ "column_" ++ (show i))) $ zip simps [1..]



codeInstantiateSelectDefault :: DBMetadata -> DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeInstantiateSelectDefault to from ss = 
  ([("I",InstantiateQuery to),("J",SectionQuery from ss)],
   Fun (instantiateSelectFName to from ss)
   (FunType [BaseType "InstanceID", t_NutleyInstance] $ tc_ErrorT t_IO $ t_NutleyInstance)
   $ Lam (Mlp [Ltp "instID", Ltp "fromInstance"])
   $ Do [(Ltp "tups", c_1 ("J." ++ sectionFName from ss) $ Lit "fromInstance"),
         (USp,c_2 ("I." ++ instantiateFName to) (Lit "instID") $ c_map (Lam (nTupPat n) (tupMap n (Lit "Just"))) (Lit "tups"))]
   )
  where n = length $ simpleRecordCompressionSchemes to
  