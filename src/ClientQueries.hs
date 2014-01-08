module ClientQueries where

import qualified Data.Map as Map
import Control.Monad

import Types
import Schema
import HaskellCode

data ClientQuery = LetQuery Name CreateQuery
                 | Show Name
                 | Select [Simplex] InstanceQuery
                   deriving (Eq,Show,Ord)

data CreateQuery = CreateSchema [TypeDec] [[Name]]
                 | InstantiateSchema SchemaQuery [Name] DataQuery
                   deriving (Eq,Show,Ord)

data InstanceQuery = NamedInstance Name
                   deriving (Eq,Show,Ord)

data SchemaQuery = NamedSchema Name
                   deriving (Eq,Show,Ord)

data DataQuery = ExplicitTuples [[Maybe String]]
               deriving (Eq,Show,Ord)
                        
data TypeDec = TypeDec Name HaskellType
             deriving (Eq,Ord)
            
instance Show TypeDec where
  show (TypeDec n t) = n ++ " : " ++ (show t)

  
