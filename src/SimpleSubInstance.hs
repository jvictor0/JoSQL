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
import NutleyQueryUtils

-- TODO: parameterization?
subInstance simp f inner = SimpleSubInstanceMetadata
  {
    simpleSubInstanceSimplex = simp,
    simpleSubInstanceName = "subinst_" ++ (name inner),
    simpleSubInstanceParamTypes = [],
    simpleSubInstanceFilterFunc = f,
    simpleSubInstanceInnerMetadata = inner
  }

codeSimpleSubInstanceMaterialize :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSimpleSubInstanceMaterialize metadata ss@(SubSchema simps schema) = 
  ([("I",MaterializeQuery (simpleSubInstanceInnerMetadata metadata) ss)],
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "SimpleSubInstance" [Ltp "params", Ltp "instID"]) 
   $ Whr (Do [materializeFun, (USp, result)])
   $ (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Left t)) $ zip [1..] (simpleSubInstanceParamTypes metadata)) ++
   [(Tup $ map (\i -> Ltp $ "_param_" ++ (show i)) [1..(length $ simpleSubInstanceParamTypes metadata)],
       Right $ c_2 "fromEitherUnsafe" (Lit "\"Parameterization Decode Error\"") $ c_1 "decode" $ Lit "params")])
  where materializeFun = (Tup $ map (\(_,i) -> Ltp $ "column_" ++ (show i)) $ zip simps [1..], 
                          (Lit $ "I." ++ (materializeFName (simpleSubInstanceInnerMetadata metadata) ss)) $$ [Lit "instID"])
        result = c_return $ Tpl $ 
                 map (\(simp,i)
                      -> if (simpleSubInstanceSimplex metadata) `subset` simp
                         then c_filter ((simpleSubInstanceFilterFunc metadata)+.+(tupNatN simp (simpleSubInstanceSimplex metadata)))
                              (Lit $ "column_" ++ (show i))
                         else Lit $ "column_" ++ (show i))
                 $ zip simps [1..]

  
