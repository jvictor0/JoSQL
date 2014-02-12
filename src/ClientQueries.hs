module ClientQueries where

import qualified Data.Map as Map
import Control.Monad

import Name
import Types
import Schema
import HaskellCode
import Utils

data ClientQuery = LetQuery Name CreateQuery
                 | ShowQuery CreateQuery
                 | SelectQuery [[Name]] InstanceQuery
--                 | KILLServer
                 | ClearCache
                 | ClearData
                 | Quit
                   deriving (Eq)

instance Show ClientQuery where
  show (LetQuery n c) = "let " ++ (show n) ++ " = " ++ (show c)
  show (ShowQuery c) = "show " ++ (show c)
  show (SelectQuery nm inst) = "select {" ++ (cim "," (\s -> "{" ++ (cim "," id s) ++ "}") nm) ++ "} from " ++ (show inst)
  show ClearCache = "clear cache"
  show ClearData = "clear data"
  show Quit = "quit" 

data FunctorType = ShriekFunctor | InverseImageFunctor | DirectImageFunctor deriving (Eq)

instance Show FunctorType where
  show ShriekFunctor = "shriek"
  show InverseImageFunctor = "inverse image"
  show DirectImageFunctor = "direct image"

data CreateQuery = CreateSchema [TypeDec] [[Name]]
                 | CreateMap SchemaQuery SchemaQuery [(Name,Name,HaskellCode)]
                 | InstantiateSchema SchemaQuery [Name] DataQuery
                 | FilterQuery InstanceQuery HaskellCode
                 | FunctorQuery FunctorType MapQuery InstanceQuery
                 | UnionQuery [InstanceQuery]
                 | NamedObject Name
                 | SchemaQuery SchemaQuery
                 | ConnectQuery String Int
                 | OnConnection ConnectQuery String
                   deriving (Eq,Show)

{- I do want to make CreateQuery have a show that looks like the user level show, but I'm bored and don't need it yet.  
instance Show CreateQuery where
  show (CreateSchema typedecs simps) = "create schema with vertices = {" 
                                       ++ (cim "," show typedecs) ++ "} simplices = {" 
                                       ++ (cim "," (\s -> "{" ++ (cim "," id s) ++ "}") simps) ++ "}"
  show (CreateMap src trg f) = "create map " ++ (show src) ++ " -> " (show trg) ++ " with {"
                               ++ (cim "," (\(a,b,f) -> a ++ " -> " ++ b ++ " by " ++ (show f))) ++ "}"
-}

data InstanceQuery = NamedInstance Name
                   | CreateInstance CreateQuery
                   deriving (Eq,Show)

data SchemaQuery = NamedSchema Name
                 | SchemaOf InstanceQuery
                   deriving (Eq,Show)

data MapQuery = NamedMap Name
              deriving (Eq,Show)

data ConnectQuery = NamedConnect Name
                  | AddressedConnect String Int
                  deriving (Eq,Show)

data DataQuery = ExplicitTuples [[Maybe String]]
               | LoadCSV FilePath
               | SelectData ClientQuery
               deriving (Eq,Show)
                        
data TypeDec = TypeDec Name HaskellType
             deriving (Eq,Ord)
            
instance Show TypeDec where
  show (TypeDec n t) = n ++ " :: " ++ (show t)

  
instance Named SchemaQuery where
  name (NamedSchema n) = n
