{-# LANGUAGE DeriveGeneric #-}
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


codeInverseImageMaterialize metadata ss = 
  ([("I",MaterializeQuery (inverseImageInnerMetadata metadata) ss')],
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "InverseImageInstance" [Ltp "params",
                                      Ltp "instID"]) 
   $ Whr
   (Do [(Ltp "preresult",c_1 innerMatName $ Lit "instID"),
        do_return $ Tpl $ 
        map (\(i,simp_fns) -> c_map (tupMaps simp_fns) $ tupNat n i $$ [Lit "preresult"])
        $ zip [1..] $ map (map (mapVertexFunc f)) $ subSchemaSimplices ss
       ])
   $ (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Left t)) $ zip [1..] (simpleSubInstanceParamTypes metadata)) ++
   [(Tup $ map (\i -> Ltp $ "_param_" ++ (show i)) [1..(length $ simpleSubInstanceParamTypes metadata)],
     Right $ c_2 "fromEitherUnsafe" (Lit "\"Parameterization Decode Error\"") $ c_1 "decode" $ Lit "params")])
  where innerMatName = "I." ++ (materializeFName (inverseImageInnerMetadata metadata) ss')
        ss' = schemaImage ss f
        simps = subSchemaSimplices ss'
        n = length simps
        f = inverseImageMap metadata
           