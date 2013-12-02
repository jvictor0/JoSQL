{-# LANGUAGE DeriveGeneric #-}
module CoLimit where

import Data.Maybe
import Data.List
import Data.Serialize
import GHC.Generics


import Utils
import HaskellCode
import NutleyInstance
import Metadata
import Name
import Schema
import Types
import TupleUtils

data NullMetadata = NullMetadata
                    {
                      nullSchema :: Schema,
                      nullName :: Name
                    }
                    deriving (Generic)
                    
instance Named NullMetadata where name = nullName

instance Serialize NullMetadata

instance DBMetadata NullMetadata where
  dbSchema = nullSchema
  instanceType _ = BaseType "()"
  codeMaterialize metadata ss =
    ([],
     Fun (materializeFName metadata ss) (materializeType metadata ss)
     $ Lam (Ltp "()") $ c_return $ Lst [])
  codeSection metadata ss =
    ([],
     Fun (sectionFName metadata ss) (sectionType metadata ss)
     $ Lam (Ltp "()") $ c_return $ Lst [])
    
data CoLimitMetadata carmd cdrmd = CoLimitMetadata 
                                   {
                                     coLimitName :: Name,
                                     coLimitCarMetadata :: carmd,
                                     coLimitCdrMetadata :: cdrmd
                                   }
                                 deriving (Generic)
                         
instance Named (CoLimitMetadata md cdrmd) where name = coLimitName

instance (Serialize md, Serialize cdrmd) => Serialize (CoLimitMetadata md cdrmd)  

instance (DBMetadata carmd, DBMetadata cdrmd) => DBMetadata (CoLimitMetadata carmd cdrmd) where
  dbSchema smd = dbSchema $ coLimitCarMetadata smd
  
  instanceType md = tc_CoLimit (instanceType $ coLimitCarMetadata md) 
                    $ instanceType $ coLimitCdrMetadata md

  codeMaterialize metadata ss =
    ([("I",MaterializeQuery (coLimitCarMetadata metadata) ss),
      ("J",MaterializeQuery (coLimitCdrMetadata metadata) ss)],
     Fun (materializeFName metadata ss) (materializeType metadata ss)
     $ Lam (Fnp "CoLimit" [Ltp "carLst",Ltp "cdr"]) 
     $ Do
     [
       (Ltp "carMats",
        c_mapM (Lit $ "I." ++ (materializeFName (coLimitCarMetadata metadata) ss)) $ Lit "carLst"),
       (Ltp "cdrMat",
        c_1 ("J." ++ (materializeFName (coLimitCarMetadata metadata) ss)) $ Lit "carLst"),
       do_return $ tupConcat (length $ subSchemaSimplices ss) $ (Lit "[cdrMat]") ++++ (Lit "carMats")
     ])
