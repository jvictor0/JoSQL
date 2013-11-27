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

data InverseImageMetadata md = InverseImageMetadata 
                               {
                                 inverseImageMap  :: SchemaMap,
                                 inverseImageName :: Name,
                                 inverseImageParamTypes :: [HaskellType],                                      
                                 inverseImageInnerMetadata :: md
                               }
                               deriving (Generic)
                         
instance Named (InverseImageMetadata md) where name = inverseImageName

instance (Serialize md) => Serialize (InverseImageMetadata md)

instance (DBMetadata md) => DBMetadata (InverseImageMetadata md) where
  dbSchema smd = schemaMapDomain $ inverseImageMap smd
  
  instanceType md = tc_InverseImage (TupType $ inverseImageParamTypes md) 
                    $ instanceType $ inverseImageInnerMetadata md
  
  codeMaterialize metadata ss = 
    ([("I",MaterializeQuery (inverseImageInnerMetadata metadata) ss')],
     Fun (materializeFName metadata ss) (materializeType metadata ss)
     $ Lam (Fnp "InverseImageInstance" [Tup $ map (\i -> Ltp $ "_param_" ++ (show i)) [1..(length $ inverseImageParamTypes metadata)],
                                   Ltp "instID"]) 
     $ Do [(Ltp "preresult",c_1 innerMatName $ Lit "instID"),
           do_return $ Tpl $ 
           map (\(i,simp_fns) -> c_map (tupMaps simp_fns) $ tupNat n i $$ [Lit "preresult"])
           $ zip [1..] $ map (map (mapVertexFunc f)) $ subSchemaSimplices ss
          ])
     where innerMatName = "I." ++ (materializeFName (inverseImageInnerMetadata metadata) ss')
           ss' = schemaImage ss f
           simps = subSchemaSimplices ss'
           n = length simps
           f = inverseImageMap metadata
           