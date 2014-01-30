module SerializeCode where

import Data.List
import Data.Serialize
import GHC.Generics

import Name
import Schema
import HaskellCode
import NutleyInstance
import Types
import Join
import Metadata
import NutleyQuery
import SimpleRecord

serializedName q = (name q) ++ "_serialize"
stringResultName q = (name q) ++ "_stringResult"
stringsInstantiateName q = (name q) ++ "_stringsInstantiate"

serializeCode :: NutleyQuery -> [HaskellFunction]
serializeCode q@(InstantiateQuery db) = 
  [{-Fun (serializedName q)  
   (FunType [BaseType "InstanceID", t_LazyByteString] $ tc_IO $ t_NutleyInstance)
   $ Lam (Mlp [Ltp "instID",  Ltp "inData"]) 
   $ (c_1 (name q) (Lit "instID")) +=<<+  (c_1 "fromEither" $ c_1 "decodeLazy" $ Lit "inData"),-}
   codeSimpleRecordStrListsToTuple db,
   Fun (stringsInstantiateName q) 
   (FunType [BaseType "InstanceID", tc_List $ tc_List $ tc_Maybe $ t_String] $ tc_ErrorT t_IO t_NutleyInstance)
   $ Lam (Mlp [Ltp "instID",  Ltp "inData"]) 
   $ Do
   [(Ltp "tps",c_1 "EitherT" $ c_1 "return" $ c_1 (strListToTupleName db) $ Lit "inData"),
    (USp,c_2 (name q) (Lit "instID") $ Lit "tps")]
   ]
serializeCode q@(SectionQuery _ _) = 
  [{-Fun (serializedName q)  
   (FunType [t_NutleyInstance] $ tc_IO $ t_LazyByteString)
   $ Lam (Mlp [Ltp "instID"]) 
   $ c_2 "fmap" (Lit "encodeLazy") $ c_1 (name q) $ Lit "instID",-}
   Fun (stringResultName q)  
   (FunType [t_NutleyInstance] $ tc_ErrorT t_IO $ t_String)
   $ Lam (Mlp [Ltp "instID"]) 
   $ c_2 "fmap" (c_2 "cim" (Lit "\"\\n\"") (Lit "show")) $ c_1 (name q) $ Lit "instID"]
serializeCode _ = []
                  