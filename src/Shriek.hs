{-# LANGUAGE DeriveGeneric #-}
module Shriek where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics

import HaskellCode
import NutleyInstance
import Metadata
import Name
import Schema
import Types
import TupleUtils

data ShriekMetadata md = ShriekMetadata 
                         {
                           shriekMap  :: SchemaMap,
                           shriekName :: Name,
                           shriekInnerMetadata :: md
                         }
                         deriving (Generic)
                         
instance Named (ShriekMetadata md) where name = shriekName
                                         
instance (Serialize md) => Serialize (ShriekMetadata md)
                                         
instance (DBMetadata md) => DBMetadata (ShriekMetadata md) where
  dbSchema smd = schemaMapCoDomain $ shriekMap smd
  
  instanceType md = tc_Shriek $ instanceType $ shriekInnerMetadata md
  
  codeSection metadata ss = 
    ([SectionQuery (shriekInnerMetadata metadata) ss'],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ if (schemaFullImage $ shriekMap metadata) `containsSubSchema` ss
       then Lam (Fnp "Shriek" [Ltp "instID"]) 
            $ c_1 innerSecName $ Lit "InstID"
       else c_1 "const" $ c_return $ Lst [])
    where innerSecName = sectionFName (shriekInnerMetadata metadata) ss'
          ss'          = SubSchema (map (fromJust.(simplexIncluded (shriekMap metadata))) $ subSchemaSimplices ss)
                         $ schemaMapDomain $ shriekMap metadata
          
  codeMaterialize metadata ss = 
    ([MaterializeQuery  (shriekInnerMetadata metadata) ss'],
     Fun (materializeFName metadata ss) (materializeType metadata ss)
     $ Lam (Fnp "Shriek" [Ltp "instID"])
     $ Do 
     [(nTupPat $ n-1, c_1 innerMatName $ Lit "instID"),
      do_return $ Tpl $ map (\(i,s) -> case i of
                                    Nothing -> Lst []
                                    (Just j) -> Lit $ "x_" ++ (show j))
           observedSimps])
    where (n,observedSimps) = mapAccumL (\i s ->
                                        if ((schemaFullImage $ shriekMap metadata) `containsSimplex` s) 
                                        then (i+1,(Just i,s))
                                        else (i,(Nothing,s)))
                            1 $ subSchemaSimplices ss
          usableSimps = map snd $ filter (isJust.fst) observedSimps
          innerMatName = sectionFName (shriekInnerMetadata metadata) ss'
          ss' =  SubSchema (map (fromJust.(simplexIncluded (shriekMap metadata))) $ usableSimps)
                 $ schemaMapDomain $ shriekMap metadata
  