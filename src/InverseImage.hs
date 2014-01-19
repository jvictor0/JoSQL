module InverseImage where

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
import Parameterize
import qualified Crypto.Hash.SHA256 as SHA


inverseImage :: SchemaMap -> DBMetadata -> Error (DBMetadata,NutleyParams)
inverseImage f md = do
  let (f',params) = parameterizeMap f
  ptypes <- mapM paramType params
  return (
    InverseImageMetadata 
    {
      inverseImageName = "invim_" ++ (name md),
      inverseImageMap  = f',
      inverseImageParamTypes = ptypes,                                      
      inverseImageInnerMetadata = md,
      inverseImageHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init [dbHashCode md,encode f',encode ptypes]
    },
    params)



codeInverseImageMaterialize metadata ss = 
  ([("I",MaterializeQuery (inverseImageInnerMetadata metadata) ss')],
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "InverseImage" [Lstp $ map (\i -> Ltp $ "pre_param_" ++ (show i)) [1..(length $ inverseImageParamTypes metadata)],
                                      Ltp "instID"]) 
   $ Whr
   (Do [(Ltp "preresult",c_1 innerMatName $ Lit "instID"),
        do_return $ Tpl $ 
        map (\(i,simp_fns) -> c_map (tupMaps simp_fns) $ tupNat n i $$ [Lit "preresult"])
        $ zip [1..] $ map (map (mapVertexFunc f)) $ subSchemaSimplices ss
       ])
   $ (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Left t)) $ zip [1..] (inverseImageParamTypes metadata)) ++
     (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Right $ c_1 "extract" $ Lit $ "pre_param_" ++ (show i)))
      $ zip [1..] (inverseImageParamTypes metadata)))
  where innerMatName = "I." ++ (materializeFName (inverseImageInnerMetadata metadata) ss')
        ss' = schemaImage ss f
        simps = subSchemaSimplices ss'
        n = length simps
        f = inverseImageMap metadata
           