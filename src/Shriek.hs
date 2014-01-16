{-# LANGUAGE DeriveGeneric #-}
module Shriek where

import Data.Tuple.HT
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
import NutleyQueryUtils

shriek f md = ShriekMetadata
  { 
    shriekMap = f,
    shriekName = "shriek_" ++ (name md),
    shriekInnerMetadata = md
  } 
                                  
codeShriekSection metadata ss = 
  (if fromInner then [("I",SectionQuery (shriekInnerMetadata metadata) ss')] else [],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ if fromInner
     then Lam (Fnp "Shriek" [Ltp "instID"]) 
          $ c_1 innerSecName $ Lit "instID"
     else c_1 "const" $ c_return $ Lst [])
  where innerSecName = "I." ++ (sectionFName (shriekInnerMetadata metadata) ss')
        ss'          = SubSchema (map (fromJust.(simplexIncluded (shriekMap metadata))) $ subSchemaSimplices ss)
                       $ schemaMapDomain $ shriekMap metadata
        fromInner = (schemaFullImage $ shriekMap metadata) `containsSubSchema` ss
        
codeShriekMaterialize metadata ss = 
  ([("I",MaterializeQuery  (shriekInnerMetadata metadata) ss')],
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
        innerMatName = "I." ++ (sectionFName (shriekInnerMetadata metadata) ss')
        ss' = SubSchema (map (fromJust.(simplexIncluded (shriekMap metadata))) $ usableSimps)
              $ schemaMapDomain $ shriekMap metadata
  
