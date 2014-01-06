module ClientQueries where

import qualified Data.Map as Map
import Control.Monad

import Types
import Schema
import HaskellCode

data ClientQuery = CreateSchema Name [TypeDec] [[Name]]
                 | Show Name
                 | InstantiateSchema SchemaQuery [Name] DataQuery
                 | Select [Simplex] InstanceQuery
                   deriving (Eq,Show,Ord)

data InstanceQuery = NamedInstance Name
                   deriving (Eq,Show,Ord)

data SchemaQuery = NamedSchema Name
                   deriving (Eq,Show,Ord)

data DataQuery = ExplicitTuples [[String]]
               deriving (Eq,Show,Ord)
                        
data TypeDec = TypeDec Name HaskellType
             deriving (Eq,Ord)
            
instance Show TypeDec where
  show (TypeDec n t) = n ++ " : " ++ (show t)

  
