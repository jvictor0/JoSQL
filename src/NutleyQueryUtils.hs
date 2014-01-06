module NutleyQueryUtils where

import Data.List

import Name
import Schema
import Types
import HaskellCode
import Utils
import Metadata
import Join

data NutleyQuery = MaterializeQuery DBMetadata SubSchema 
                 | SectionQuery DBMetadata SubSchema 
                 | InstantiateQuery DBMetadata

instance Named NutleyQuery where
  name (MaterializeQuery metadata ss) = materializeFName metadata ss
  name (SectionQuery metadata ss) = sectionFName metadata ss
  name (InstantiateQuery metadata) = instantiateFName metadata

materializeFName metadata subschema = "materialize_" ++ (name metadata) ++ "_" ++ (name subschema)
sectionFName metadata subschema = "section_" ++ (name metadata) ++ "_" ++ (name subschema)
instantiateFName metadata = "instantiate_" ++ (name metadata)

materializeType metadata ss@(SubSchema simps _) = funType
  where inTupType = TupType $ map (tc_List.TupType.(univ (dbSchema metadata))) simps
        funType = FunType [t_NutleyInstance] $ tc_IO $ inTupType
        verts = nub $ concat simps

sectionType :: DBMetadata -> SubSchema -> HaskellType
sectionType metadata (SubSchema simps _) = FunType [t_NutleyInstance] $ tc_IO $ outTupType         
  where outTupType = tc_List $ TupType $ map (typeLookup $ dbSchema metadata) allVerts
        allVerts = foldr union [] $ simps :: [VertID]


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


