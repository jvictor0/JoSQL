module ClientQueries where

import qualified Data.Map as Map
import Control.Monad

import Name
import Types
import Schema
import HaskellCode

data ClientQuery = LetQuery Name CreateQuery
                 | ShowQuery Name
                 | SelectQuery [[Name]] InstanceQuery
                 | KILLServer
                 | ClearCache
                 | ClearData
                 | Quit
                   deriving (Eq,Show)

data FunctorType = ShriekFunctor | InverseImageFunctor | DirectImageFunctor deriving (Eq,Show)

data CreateQuery = CreateSchema [TypeDec] [[Name]]
                 | CreateMap SchemaQuery SchemaQuery [(Name,Name,HaskellCode)]
                 | InstantiateSchema SchemaQuery [Name] DataQuery
                 | FilterQuery InstanceQuery HaskellCode
                 | FunctorQuery FunctorType MapQuery InstanceQuery
                 | UnionQuery [InstanceQuery]
                   deriving (Eq,Show)

data InstanceQuery = NamedInstance Name
                   | CreateInstance CreateQuery
                   deriving (Eq,Show)

data SchemaQuery = NamedSchema Name
                   deriving (Eq,Show)

data MapQuery = NamedMap Name
              deriving (Eq,Show)

data DataQuery = ExplicitTuples [[Maybe String]]
               | LoadCSV FilePath
               | SelectData ClientQuery
               deriving (Eq,Show)
                        
data TypeDec = TypeDec Name HaskellType
             deriving (Eq,Ord)
            
instance Show TypeDec where
  show (TypeDec n t) = n ++ " : " ++ (show t)

  
instance Named SchemaQuery where
  name (NamedSchema n) = n
