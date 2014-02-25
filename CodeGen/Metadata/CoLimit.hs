module CodeGen.Metadata.CoLimit where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics
import Data.Tuple.HT
import Control.Monad.Trans.Either



import Utils.Utils
import CodeGen.HaskellCode
import Server.NutleyInstance
import CodeGen.Metadata.Metadata
import Data.Name
import Data.Schema
import Data.Types
import CodeGen.TupleUtils
import CodeGen.Metadata.Shriek
import CodeGen.NutleyQueryUtils                
import qualified Crypto.Hash.SHA256 as SHA


coLimitOne :: [(DBMetadata,NutleyInstance)] -> (DBMetadata,NutleyInstance)
coLimitOne inners' = 
  let inners'' :: [(DBMetadata,[NutleyInstance])]
      inners'' = concatMap (\(db,inst) -> case inst of
                               (CoLimit ins) -> zipWith (,) (coLimitInnerMetadatas db) ins
                               _             -> [(db,[inst])])
                 inners'
      inners = map (\((md,c):cs) -> (md,c ++ (concatMap snd cs)))
               $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) inners''
  in 
   (
    CoLimitMetadata 
    {
      coLimitName = "coprd_" ++ (name $ fst $ head inners),
      coLimitInnerMetadatas = map fst inners,
      coLimitHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init $ map (dbHashCode.fst) inners
    },
    CoLimit $ map snd inners
   )
                    
coProduct :: [DBMetadata] -> DBMetadata
coProduct inners = CoLimitMetadata
  {
    coLimitName = cim "_coprd_" name inners,
    coLimitInnerMetadatas = zipWith shriek incs inners,
    coLimitHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init $ map dbHashCode inners
  }
  where (coProdSchema,incs) = schemaCoProduct $ map dbSchema inners
        
coLimitInnerMaterializeQueries db ss = 
  map (\(i,imd) -> ("imp" ++ (show i), MaterializeQuery imd ss,"ins_" ++ (show i))) 
  $ filter ((nontrivialMaterialization ss).snd) $ zip [1..] $ coLimitInnerMetadatas db


codeCoLimitMaterialize metadata ss =
  (map (\(x,y,_) -> (x,y)) innerMats,
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "CoLimit" $ [Lstp $ map Ltp $ map (\(i,_) -> "ins_" ++ (show i)) $ zip [1..] $ coLimitInnerMetadatas metadata])
   $ Do 
   $ (map (\(i,(mod,q,nms)) -> (Ltp $ "kinz_" ++ (show i), 
                                c_mapM (Lit $ mod ++ "_" ++ (name q)) $ Lit nms))
      $ zip [1..] innerMats) ++ 
     [do_return $ tupConcat (length $ subSchemaSimplices ss) $ c_concat $ Lst $ map (\(i,_) -> Lit $ "kinz_" ++ (show i)) 
      $ zip [1..] innerMats]
   )
  where innerMats = coLimitInnerMaterializeQueries metadata ss
