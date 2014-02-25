{-# LANGUAGE DeriveGeneric #-}
module CodeGen.Metadata.Shriek where

import Data.Tuple.HT
import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics

import CodeGen.HaskellCode
import Server.NutleyInstance
import CodeGen.Metadata.Metadata
import Data.Name
import Data.Schema
import Data.Types
import CodeGen.TupleUtils
import CodeGen.NutleyQueryUtils
import qualified Crypto.Hash.SHA256 as SHA


shriek f md = ShriekMetadata
  { 
    shriekMap = f,
    shriekName = "shriek_" ++ (name md),
    shriekInnerMetadata = md,
    shriekHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init [dbHashCode md,encode f]
  } 
                                  
codeShriekSection metadata ss = 
  (if fromInner then [("i",SectionQuery (shriekInnerMetadata metadata) ss')] else [],
   Fun (sectionFName metadata ss) (sectionType metadata ss)
   $ if fromInner
     then Lam (Fnp "Shriek" [Ltp "instID"]) 
          $ c_1 innerSecName $ Lit "instID"
     else c_1 "const" $ c_return $ Lst [])
  where innerSecName = "i_" ++ (sectionFName (shriekInnerMetadata metadata) ss')
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
        innerMatName = "i_" ++ (sectionFName (shriekInnerMetadata metadata) ss')
        ss' = SubSchema (map (fromJust.(simplexIncluded (shriekMap metadata))) $ usableSimps)
              $ schemaMapDomain $ shriekMap metadata
  
