module SimpleRecord where

import qualified Data.Array.BitArray as BA
import Data.List
import Data.Maybe

import Name
import Schema
import Types
import HaskellCode
import QueryCompState
import Utils
import TupleUtils


data SimpleRecordMetadata = SimpleRecordMetadata 
                            {
                              simpleRecordSchema :: ConnectedSchema,
                              simpleRecordCompressionSchemes   :: [(SimpID, HaskellCode)],
                              simpleRecordDecompressionSchemes :: [(SimpID, HaskellCode)]
                            }
data SimpleRecord = SimpleRecord InstanceID RowCount deriving (Eq, Ord)

segmentFileName name v = Lit $ "\"segment_" ++ name ++ "_" ++ (show v) ++ "_\" ++ (show instID) ++ \".seg\""

instantiateSimpleRecordFName n = "instantiateSimpleRecord_" ++ n

codeSimpleRecordInstantiate :: SimpleRecordMetadata -> HaskellFunction
codeSimpleRecordInstantiate srmd = 
  Fun (instantiateSimpleRecordFName name)  funType 
  (Lam (Mlp [Ltp "instID",  Ltp "inData"]) $ 
   Whr (Do
        [(USp, writeFiles),
         (USp,c_return $ Lit $ "SimpleRecord instanceID (length inData)")])
   $ columnCodes ++ compressedColumnCodes ++ fileNameCodes)
  where s@(Schema name verts _) = simpleRecordSchema srmd
        inTupType = listTC $ TupType $ map (maybeTC . BaseType . snd) verts
        inTupName = Lit "inData"
        inInstanceName = Lit "instID"
        funType = FunType [instanceID, inTupType] $ ioTC $ BaseType "SimpleRecord"
        
        numCols = length verts
        columnCodes = map (\((v,_),i) -> 
                            (Ltp $"column_" ++ (show v) , c_map (tupNat numCols i) inTupName))
                      $ zip verts [1..]
        compressedColumnCodes = map (\(v,compsh) -> 
                                      (Ltp $ "compColumn_" ++ (show v) , compsh $$ [Lit $ "column_" ++ (show v)]))
                                $ simpleRecordCompressionSchemes srmd
        
        fileNameCodes = map (\(v,_) -> 
                              (Ltp $ "segmentFile_" ++ (show v) , 
                               segmentFileName name v))
                        verts
        writeFiles = c_mapM_ (Lit "(uncurry writeFile)")  $ 
                     (Lit $ "[" ++ (cim "," (\v -> "(segmentFile_" ++ (show v) ++ ",compColumn_" ++ (show v) ++ ")") $ map fst verts) ++ "]")


codeSimpleRecordConndSection :: SimpleRecordMetadata -> SubSchema -> HaskellFunction
codeSimpleRecordConndSection metadata ss@(SubSchema _ simps schema) = 
  Fun (name ss) funType
  $ Lam (Ltp "instID")
  $ Do (columnInstantiations ++ [tupsCode])
    where columnIDs = map head $ group $ concat $ transpose $ map (vertices schema) simps
          columnInstantiations = map (\cid -> 
                                       (Ltp $ "column_" ++ (show cid),
                                        (c_2 "fmap" 
                                         (fromJust $ lookup cid $ simpleRecordDecompressionSchemes metadata)
                                         (c_1 "readFile" (segmentFileName (name schema) cid)))))
                                 $ columnIDs 
          n = length columnIDs
          tupsCode = (USp,c_return $ c_mapMaybe (maybeTup n) $ zipN $ map (\cid -> Lit $ "column_" ++ (show cid)) columnIDs)
          
          inTupType = listTC $ TupType $ map (BaseType.(typeLookup schema)) columnIDs
          funType = FunType [BaseType "SimpleRecord"] $ ioTC $ inTupType

