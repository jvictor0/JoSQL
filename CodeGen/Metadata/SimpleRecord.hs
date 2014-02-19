module CodeGen.Metadata.SimpleRecord where

import Data.List
import Data.Maybe
import Data.Serialize
import GHC.Generics

import Data.Name
import Data.Schema
import Data.Types
import CodeGen.HaskellCode
import Utils.Utils
import CodeGen.TupleUtils
import Server.NutleyInstance
import CodeGen.Metadata.Metadata
import CodeGen.NutleyQueryUtils
import qualified Crypto.Hash.SHA256 as SHA


                                          
-- NOTE: if you add more encodings, remember to update hash!
simpleRecord :: Name -> Schema -> [VertID] -> DBMetadata
simpleRecord name sch simp = SimpleRecordMetadata 
  {
    simpleRecordName = name,
    simpleRecordSchema = sch,
    simpleRecordCompressionSchemes = map (\v -> (v,Lit "encodeLazy")) simp,
    simpleRecordDecompressionSchemes = map (\v -> (v,Lit "decodeLazy")) simp,
    simpleRecordHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init [encode name,encode sch, encode simp]
  }

segmentFileName md v = Lit $ "\"Segments/segment_" ++ ((name md) ++ "_" ++ (show v) ++ "_\" ++ (show instID) ++ \".seg\"")

simpleRecordSimplex = (map fst).simpleRecordCompressionSchemes
strListToTupleName srmd = "strListToTuple_" ++ (name srmd)

codeSimpleRecordStrListsToTuple :: DBMetadata -> HaskellFunction
codeSimpleRecordStrListsToTuple srmd =
  Fun (strListToTupleName srmd)
  (FunType [tc_List $ tc_List $ tc_Maybe $ t_String] $ tc_1 "Error" $ tc_List $ TupType $ map (tc_Maybe . snd) vertTypes) 
  $ Lam (Ltp "ins")
  $ Whr (c_mapM (Lit "pTup") (Lit "ins"))
  [(FnpNP "pTup" [Lstp $ map (\(_,i) -> Ltp $ "z_" ++ (show i)) $ zip simplex [1..]],
    Right $ eitherTup (length simplex) "Parse error in tuple" $$ 
    [Tpl $ map (\((_,t),i) -> c_1 "readJustMaybe" $ Lit $ "z_" ++ (show i)) 
     $ zip vertTypes [1..]]),
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
        compSchemes = map (\v -> (v, fromJustE "compression lookup fail" $ lookup v $ simpleRecordCompressionSchemes srmd)) simplex
        inTupType = tc_List $ TupType $ map (tc_Maybe . snd) vertTypes
        inTupName = Lit "inData"
        inInstanceName = Lit "instID"
        funType = FunType [BaseType "InstanceID", inTupType] $ tc_ErrorT t_IO $ t_NutleyInstance
        
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
        writeFiles = c_1 "EitherT" $ c_2 "fmap" (Lit "return") $ c_mapM_ (Lit "(uncurry writeByteStringFile)")  $ 
                     (Lit $ "[" ++ (cim "," (\v -> "(segmentFile_" ++ (show v) ++ ",compColumn_" ++ (show v) ++ ")") $ verts) ++ "]")


codeSimpleRecordMaterialize :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSimpleRecordMaterialize metadata ss@(SubSchema simps sch) =
  ([],
   Fun (materializeFName metadata ss) funType
   $ Lam (Fnp "SimpleRecord" [Ltp "instID",USp])
   $ Do (columnInstantiations ++ [tupsCode]
        ))
    where columnInstantiations = map (\cid -> 
                                       case lookup cid $ simpleRecordDecompressionSchemes metadata of
                                         Nothing -> do_return $ Lit "()"
                                         (Just decomp) -> 
                                           (Ltp $ "column_" ++ (show cid),
                                            c_1 "EitherT" $ c_2 "fmap" decomp
                                            $ c_1 "readByteStringFile" $ segmentFileName metadata cid))
                                 $ verts
          tupsCode = (USp,c_return $ Tpl $ map 
                          (\s -> if s`subset`(simpleRecordSimplex metadata)
                                 then c_mapMaybe (maybeTup (length s)) $ zipN $ map (\v -> Lit $ "column_" ++ (show v)) s
                                 else Lit "[]")
                          simps)
          funType = materializeType metadata ss
          verts = nub $ concat $ filter (`subset`(simpleRecordSimplex metadata)) simps

