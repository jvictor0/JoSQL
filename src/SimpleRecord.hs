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
import NutleyQueryUtils

                                          
simpleRecord :: Name -> Schema -> [VertID] -> DBMetadata
simpleRecord name sch simp = SimpleRecordMetadata 
  {
    simpleRecordName = name,
    simpleRecordSchema = sch,
    simpleRecordCompressionSchemes = map (\v -> (v,Lit "encodeLazy")) simp,
    simpleRecordDecompressionSchemes = map (\v -> (v,Lit "decodeLazy")) simp
  }

segmentFileName md v = Lit $ "\"segment_" ++ ((name md) ++ "_" ++ (show v) ++ "_\" ++ (show instID) ++ \".seg\"")

simpleRecordSimplex = (map fst).simpleRecordCompressionSchemes
strListToTupleName srmd = "strListToTuple_" ++ (name srmd)

codeSimpleRecordStrListsToTuple :: DBMetadata -> HaskellFunction
codeSimpleRecordStrListsToTuple srmd =
  Fun (strListToTupleName srmd)
  (FunType [tc_List $ tc_List $ tc_Maybe $ t_String] $ tc_Either t_String $ tc_List $ TupType $ map (tc_Maybe . snd) vertTypes) 
  $ Lam (Ltp "ins")
  $ Whr (c_mapM (Lit "pTup") (Lit "ins"))
  [(FnpNP "pTup" [Lstp $ map (\(_,i) -> Ltp $ "z_" ++ (show i)) $ zip simplex [1..]],
    Right $ eitherTup (length simplex) "Parse error in tuple" $$ 
    [Tpl $ map (\(_,i) -> c_1 "readJustMaybe" $ Lit $ "z_" ++ (show i)) $ zip simplex [1..]]),
   (FnpNP "pTup" [USp], Right $ c_1 "Left" $ Lit $ "\"Tuple length mismatch\"")]
  where s = simpleRecordSchema srmd
        simplex = simpleRecordSimplex srmd
        vertTypes = zip simplex $ univ s simplex



codeSimpleRecordInstantiate :: DBMetadata -> HaskellFunction
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
        funType = FunType [BaseType "InstanceID", inTupType] $ tc_IO $ t_NutleyInstance
        
        numCols = length verts
        columnCodes = map (\(v,i) -> 
                            (Ltp $"column_" ++ (show v) , Right $ c_map (tupNat numCols i) inTupName))
                      $ zip verts [1..]
        compressedColumnCodes = map (\(v,compsh) -> 
                                      (Ltp $ "compColumn_" ++ (show v) , Right $  compsh $$ [Lit $ "column_" ++ (show v)]))
                                $ compSchemes
        
        fileNameCodes = map (\v -> 
                              (Ltp $ "segmentFile_" ++ (show v) , Right $ segmentFileName srmd v))
                        verts
        writeFiles = c_mapM_ (Lit "(uncurry writeByteStringFile)")  $ 
                     (Lit $ "[" ++ (cim "," (\v -> "(segmentFile_" ++ (show v) ++ ",compColumn_" ++ (show v) ++ ")") $ verts) ++ "]")


codeSimpleRecordMaterialize :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
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

