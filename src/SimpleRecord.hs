{-# LANGUAGE DeriveGeneric #-}
module SimpleRecord where

import Data.List
import Data.Maybe
import Data.Serialize
import GHC.Generics

import Name
import Schema
import Types
import HaskellCode
import QueryCompState
import Utils
import TupleUtils
import NutleyInstance
import Metadata

data SimpleRecordMetadata = SimpleRecordMetadata 
                            {
                              simpleRecordSchema :: Schema,
                              simpleRecordName :: String,
                              simpleRecordCompressionSchemes   :: [(VertID, HaskellCode)],
                              simpleRecordDecompressionSchemes :: [(VertID, HaskellCode)]
                            }
                            deriving (Generic)

instance Named SimpleRecordMetadata where name = simpleRecordName
                            
instance Serialize SimpleRecordMetadata
                                          
segmentFileName md v = Lit $ "\"segment_" ++ ((name md) ++ "_" ++ (show v) ++ "_\" ++ (show instID) ++ \".seg\"")

simpleRecordSimplex = (map fst).simpleRecordCompressionSchemes


codeSimpleRecordInstantiate :: SimpleRecordMetadata -> HaskellFunction
codeSimpleRecordInstantiate srmd = 
  Fun (instantiateFName srmd)  funType 
  (Lam (Mlp [Ltp "instID",  Ltp "inData"]) $ 
   Whr (Do
        [(USp, writeFiles),
         (USp, c_return $ Lit $ "SimpleRecord instID (length inData)")])
   $ columnCodes ++ compressedColumnCodes ++ fileNameCodes)
  where s = simpleRecordSchema srmd
        simplex = simpleRecordSimplex srmd
        verts = simplex
        vertTypes = zip simplex $ univ s simplex
        compSchemes = map (\v -> (v, fromJust $ lookup v $ simpleRecordCompressionSchemes srmd)) simplex
        inTupType = tc_List $ TupType $ map (tc_Maybe . snd) vertTypes
        inTupName = Lit "inData"
        inInstanceName = Lit "instID"
        funType = FunType [BaseType "InstanceID", inTupType] $ tc_IO $ t_SimpleRecord
        
        numCols = length verts
        columnCodes = map (\(v,i) -> 
                            (Ltp $"column_" ++ (show v) , c_map (tupNat numCols i) inTupName))
                      $ zip verts [1..]
        compressedColumnCodes = map (\(v,compsh) -> 
                                      (Ltp $ "compColumn_" ++ (show v) , compsh $$ [Lit $ "column_" ++ (show v)]))
                                $ compSchemes
        
        fileNameCodes = map (\v -> 
                              (Ltp $ "segmentFile_" ++ (show v) , 
                               segmentFileName srmd v))
                        verts
        writeFiles = c_mapM_ (Lit "(uncurry writeByteStringFile)")  $ 
                     (Lit $ "[" ++ (cim "," (\v -> "(segmentFile_" ++ (show v) ++ ",compColumn_" ++ (show v) ++ ")") $ verts) ++ "]")


codeSimpleRecordMaterialize :: SimpleRecordMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSimpleRecordMaterialize metadata ss@(SubSchema simps sch)
  | not $ null $ verts\\(simpleRecordSimplex metadata) =   
    ([],
     Fun (materializeFName metadata ss) funType
     $ Lam (Fnp "SimpleRecord" [Ltp "instID",USp]) $ c_return $ Lst [])
  where funType = materializeType metadata ss
        verts = nub $ concat simps
codeSimpleRecordMaterialize metadata ss@(SubSchema simps sch) =
  ([],
   Fun (materializeFName metadata ss) funType
   $ Lam (Fnp "SimpleRecord" [Ltp "instID",USp])
   $ Do (columnInstantiations ++ [tupsCode]
        ))
    where columnInstantiations = map (\cid -> 
                                       (Ltp $ "column_" ++ (show cid),
                                        ((c_2 "fmap" 
                                         (fromJust $ lookup cid $ simpleRecordDecompressionSchemes metadata)
                                         (c_1 "readByteStringFile" (segmentFileName metadata cid))))
                                       +>>=+ (Lit "fromEither")))
                                 $ verts
          tupsCode = (USp,c_return $ Tpl $ map 
                          (\s -> c_mapMaybe (maybeTup (length s)) $ zipN $ map (\v -> Lit $ "column_" ++ (show v)) s)
                          simps)
          funType = materializeType metadata ss
          verts = nub $ concat simps

instance DBMetadata SimpleRecordMetadata where
  dbSchema = simpleRecordSchema
  instanceType _ = t_SimpleRecord
  codeMaterialize = codeSimpleRecordMaterialize
  
instance InstantiateableDBMetadata SimpleRecordMetadata where
  codeInstantiate md = ([],codeSimpleRecordInstantiate md)