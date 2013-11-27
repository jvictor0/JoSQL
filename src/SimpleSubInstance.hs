{-# LANGUAGE DeriveGeneric #-}
module SimpleSubInstance where

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

data SimpleSubInstanceMetadata md = SimpleSubInstanceMetadata
                                    {
                                      simpleSubInstanceSimplex :: Simplex,
                                      simpleSubInstanceName :: Name,
                                      simpleSubInstanceParamTypes :: [HaskellType],
                                      simpleSubInstanceFilterFunc :: HaskellCode,
                                      simpleSubInstanceInnerMetadata :: md
                                    }
                                    deriving (Generic)
                                             
instance Named (SimpleSubInstanceMetadata md) where name = simpleSubInstanceName

instance (Serialize md) => Serialize (SimpleSubInstanceMetadata md)

codeSimpleSubInstanceMaterialize :: (DBMetadata md) => 
                                    SimpleSubInstanceMetadata md -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSimpleSubInstanceMaterialize metadata ss@(SubSchema simps schema) = 
  ([("I",MaterializeQuery (simpleSubInstanceInnerMetadata metadata) ss)],
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "SimpleSubInstance" [Tup $ map (\i -> Ltp $ "_param_" ++ (show i)) [1..(length $ simpleSubInstanceParamTypes metadata)],
                                   Ltp "instID"]) 
   $ Do [materializeFun, (USp, result)])
  where materializeFun = (Tup $ map (\(_,i) -> Ltp $ "column_" ++ (show i)) $ zip simps [1..], 
                          (Lit $ "I." ++ (materializeFName (simpleSubInstanceInnerMetadata metadata) ss)) $$ [Lit "instID"])
        result = c_return $ Tpl $ 
                 map (\(simp,i)
                      -> if (simpleSubInstanceSimplex metadata) `subset` simp
                         then c_filter ((simpleSubInstanceFilterFunc metadata)+.+(tupNatN simp (simpleSubInstanceSimplex metadata)))
                              (Lit $ "column_" ++ (show i))
                         else Lit $ "column_" ++ (show i))
                 $ zip simps [1..]

instance (DBMetadata md) => DBMetadata (SimpleSubInstanceMetadata md) where
  dbSchema md = dbSchema $ simpleSubInstanceInnerMetadata md
  instanceType md = tc_SimpleSubInstance (TupType $ simpleSubInstanceParamTypes md) 
                    (instanceType $ simpleSubInstanceInnerMetadata md)
  codeMaterialize = codeSimpleSubInstanceMaterialize
  
