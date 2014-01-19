module SimpleSubInstance where

import Data.List
import Data.Maybe
import Data.Serialize
import GHC.Generics
import qualified Data.Map as Map

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
import Parameterize
import qualified Crypto.Hash.SHA256 as SHA


subInstance :: HaskellCode -> DBMetadata -> Error (DBMetadata,NutleyParams)
subInstance f inner = do 
  let (simp,eta_f,params) = parameterize (Map.fromList $ map (\(x,y) -> (y,x)) $ dbVertexNames inner) f
  ptypes <- mapM paramType params
  return (
    SimpleSubInstanceMetadata
    {
      simpleSubInstanceSimplex = simp,
      simpleSubInstanceName = "subinst_" ++ (name inner),
      simpleSubInstanceParamTypes = ptypes,
      simpleSubInstanceFilterFunc = eta_f,
      simpleSubInstanceInnerMetadata = inner,
      simpleSubInstanceHashCode = SHA.finalize $ foldr (flip SHA.update) SHA.init [dbHashCode inner,encode eta_f,encode ptypes,encode simp]
    },
    params)


codeSimpleSubInstanceMaterialize :: DBMetadata -> SubSchema -> ([(Name,NutleyQuery)],HaskellFunction)
codeSimpleSubInstanceMaterialize metadata ss@(SubSchema simps schema) = 
  ([("I",MaterializeQuery (simpleSubInstanceInnerMetadata metadata) ss)],
   Fun (materializeFName metadata ss) (materializeType metadata ss)
   $ Lam (Fnp "SimpleSubInstance" [Lstp $ map (\i -> Ltp $ "pre_param_" ++ (show i)) [1..(length $ simpleSubInstanceParamTypes metadata)], 
                                   Ltp "instID"])
   $ Whr (Do [materializeFun, (USp, result)])
   $ (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Left t)) $ zip [1..] (simpleSubInstanceParamTypes metadata)) ++
     (map (\(i,t) -> (Ltp $ "_param_" ++ (show i),Right $ c_1 "extract" $ Lit $ "pre_param_" ++ (show i)))
      $ zip [1..] (simpleSubInstanceParamTypes metadata)))
  where materializeFun = (Tup $ map (\(_,i) -> Ltp $ "column_" ++ (show i)) $ zip simps [1..], 
                          (Lit $ "I." ++ (materializeFName (simpleSubInstanceInnerMetadata metadata) ss)) $$ [Lit "instID"])
        result = c_return $ Tpl $ 
                 map (\(simp,i)
                      -> if (simpleSubInstanceSimplex metadata) `subset` simp
                         then c_filter ((simpleSubInstanceFilterFunc metadata)+.+(tupNatN simp (simpleSubInstanceSimplex metadata)))
                              (Lit $ "column_" ++ (show i))
                         else Lit $ "column_" ++ (show i))
                 $ zip simps [1..]

  
