module GlobalState where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

import Utils
import Types
import NutleyInstance
import Schema
import ClientQueries
import NutleyQuery

data NutleyObject = NutleySchema Schema (Map.Map Name VertID) | 
                    NutleySubSchema SubSchema | 
                    NutleySchemaMap SchemaMap

data GlobalState = GlobalState 
                   {
                     namedObjects :: TVar (Map.Map Name NutleyObject),
                     clientQueries :: TVar (Map.Map ClientQuery NutleyQuery)
                   }
                   
showNutleyObject (NutleySchema (Schema (SC verts simps) tps) mp) = result
  where invmp = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList mp
        vertNamed = map (\(i,t) -> (fromJust $ Map.lookup i invmp) ++ " : " ++ (show t)) tps
        simpNamed = map (cim ", "  (fromJust . (flip Map.lookup invmp))) simps
        result = "schema with\n  vertices =\n  {\n" ++
                 (cim ",\n" ("    "++) vertNamed) ++ "\n  }\n" ++ 
                 "  simplices =\n  {\n" ++ 
                 (cim ",\n" (\x -> "    { "++x++" }") simpNamed) ++ "\n  }"

newGlobalState = atomically $ do
  no <- newTVar Map.empty
  cq <- newTVar Map.empty
  return $ GlobalState { namedObjects = no, clientQueries = cq }
                   
lookupByName :: GlobalState -> Name -> STM NutleyObject
lookupByName state name = fromMaybeM $ fmap (Map.lookup name) $ readTVar $ namedObjects state

nameExists state name = (fmap (const True) $ lookupByName state name) `orElse` (return False)
  
addNamedObject state name object = flip modifyTVar (Map.insert name object) $ namedObjects state

addNamedObjectIfNotExists state name object = do
  (guard.not) =<< (nameExists state name)
  addNamedObject state name object
  

lookupNutleyQuery :: GlobalState -> ClientQuery -> STM NutleyQuery
lookupNutleyQuery state query = fromMaybeM $ fmap (Map.lookup query) $ readTVar $ clientQueries state

clientQueryToNutleyQuery :: GlobalState -> ClientQuery -> STM NutleyQuery
clientQueryToNutleyQuery state query = retry

getNutleyQuery state query = (lookupNutleyQuery state query) `orElse` do
  result <- clientQueryToNutleyQuery state query 
  flip modifyTVar (Map.insert query result) $ clientQueries state
  return result
  
