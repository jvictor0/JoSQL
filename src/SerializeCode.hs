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

serializedName q = (name q) ++ "_serialize"

serializeCode :: NutleyQuery -> HaskellFunction
serializeCode q@(InstantiateQuery db) = 
  Fun (serializedName q)  
  (FunType [BaseType "InstanceID", t_LazyByteString] $ tc_IO $ t_ByteString)
  $ Lam (Mlp [Ltp "instID",  Ltp "inData"]) 
  $ c_2 "fmap" (Lit "encode") $ (c_1 (name q) (Lit "instID")) +=<<+  (c_1 "fromEither" $ c_1 "decodeLazy" $ Lit "inData")
serializeCode q = 
  Fun (serializedName q)  
  (FunType [t_ByteString] $ tc_IO $ t_LazyByteString)
  $ Lam (Mlp [Ltp "instID"]) 
  $ c_2 "fmap" (Lit "encodeLazy") $ (Lit $ name q) +=<<+ (c_1 "fromEither" $ c_1 "decode" $ Lit "instID")
        
                  