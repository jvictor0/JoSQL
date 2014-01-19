module CoLimit where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics
import Data.Tuple.HT


import Utils
import HaskellCode
import NutleyInstance
import Metadata
import Name
import Schema
import Types
import TupleUtils
import Shriek
import NutleyQueryUtils
                         
coProduct :: [DBMetadata] -> DBMetadata
coProduct inners = CoLimitMetadata
  {
    coLimitName = cim "_coprd_" name inners,
    coLimitInnerMetadatas = zipWith shriek incs inners
  }
  where (coProdSchema,incs) = schemaCoProduct $ map dbSchema inners
        
coLimitInnerMaterializeQueries db ss = 
  map (\(i,imd) -> ("IMP" ++ (show i), MaterializeQuery imd ss,"ins_" ++ (show i))) $ zip [1..] $ coLimitInnerMetadatas db


codeCoLimitMaterialize metadata ss =
  (map (\(x,y,_) -> (x,y)) innerMats,
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "CoLimit" $ [Lstp $ map Ltp $ map thd3 $ innerMats])
   $ Do 
   $ (map (\(i,(mod,q,nms)) -> (Ltp $ "kinz_" ++ (show i), 
                                c_mapM (Lit $ mod ++ "." ++ (name q)) $ Lit nms))
      $ zip [1..] innerMats) ++ 
     [do_return $ tupConcat (length $ subSchemaSimplices ss) $ c_concat $ Lst $ map (\(i,_) -> Lit $ "kinz_" ++ (show i)) 
      $ zip [1..] innerMats]
   )
  where innerMats = coLimitInnerMaterializeQueries metadata ss
