module GlobalState where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.Either

import Utils
import Types
import NutleyInstance
import Schema
import ClientQueries
import NutleyQuery
import Metadata
import Name

data NutleyObject = NutleySchema Schema (Map.Map Name VertID) | 
                    NutleyObjInstance NutleyInstance DBMetadata
                    
data GlobalState = GlobalState 
                   {
                     namedObjects :: TVar (Map.Map Name NutleyObject),
                     instanceIDCounter :: TVar Int,
                     clientQueries :: TVar (Map.Map ClientQuery NutleyQuery)
                   }
                   
nextInstanceID state = do
  id <- readTVar $ instanceIDCounter state
  writeTVar (instanceIDCounter state) $ id + 1
  return id
                   
showNutleyObject (NutleySchema (Schema (SC verts simps) tps) mp) = result
  where invmp = Map.fromList $ map (\(a,b) -> (b,a)) $ Map.toList mp
        vertNamed = map (\(i,t) -> (fromJust $ Map.lookup i invmp) ++ " : " ++ (show t)) tps
        simpNamed = map (cim ", "  (fromJust . (flip Map.lookup invmp))) simps
        result = "create schema with\n  vertices =\n  {\n" ++
                 (cim ",\n" ("    "++) vertNamed) ++ "\n  }\n" ++ 
                 "  simplices =\n  {\n" ++ 
                 (cim ",\n" (\x -> "    { "++x++" }") simpNamed) ++ "\n  }"
showNutleyObject (NutleyObjInstance isnt md) = "instance of database " ++ (name md)

newGlobalState = atomically $ do
  no <- newTVar Map.empty
  cq <- newTVar Map.empty
  id <- newTVar 0
  return $ GlobalState { namedObjects = no, clientQueries = cq , instanceIDCounter = id}
                   
lookupByName :: GlobalState -> Name -> ErrorT STM NutleyObject
lookupByName state name = do
  res <- liftEitherT $ fmap (Map.lookup name) $ readTVar $ namedObjects state
  case res of
    Nothing -> left $ "Object " ++ name ++ " cannot be found"
    (Just a) -> return a

nameExists state name = eitherT (const $ return False) (const $ return True) $ lookupByName state name
  
addNamedObject state name object = flip modifyTVar (Map.insert name object) $ namedObjects state

addNamedObjectIfNotExists :: GlobalState -> Name -> NutleyObject -> ErrorT STM ()
addNamedObjectIfNotExists state name object = do
  (hoistEither.(guardEither $ "Object " ++ name ++ " already exists").not) =<< (liftEitherT $ nameExists state name)
  liftEitherT $ addNamedObject state name object

lookupNutleyQuery :: GlobalState -> ClientQuery -> STM NutleyQuery
lookupNutleyQuery state query = fromMaybeM $ fmap (Map.lookup query) $ readTVar $ clientQueries state

clientQueryToNutleyQuery :: GlobalState -> ClientQuery -> STM NutleyQuery
clientQueryToNutleyQuery state query = retry

getNutleyQuery state query = (lookupNutleyQuery state query) `orElse` do
  result <- clientQueryToNutleyQuery state query 
  flip modifyTVar (Map.insert query result) $ clientQueries state
  return result
  