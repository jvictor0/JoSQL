module Server.GlobalState where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.Either
import System.IO
import Network

import Utils.Utils
import Data.Types
import Server.NutleyInstance
import Data.Schema
import Server.ClientQueries
import CodeGen.NutleyQuery
import CodeGen.Metadata.Metadata
import Data.Name
import CodeGen.HaskellCode

data NutleyObject = NutleySchema Schema 
                  | NutleyMap SchemaMap 
                  | NutleyObjInstance NutleyInstance DBMetadata
                  | NutleyActionObject (ErrorT IO NutleyObject)
                  | NutleyConnection HostName PortID
                  | EmptyNutleyObject
                    
data GlobalState = GlobalState 
                   {
                     namedObjects :: TVar (Map.Map Name NutleyObject),
                     instanceIDCounter :: TVar Int                   
                   }
                   
isActionObject (NutleyActionObject _) = True
isActionObject _ = False

execNutleyInstance :: NutleyObject -> ErrorT IO (NutleyInstance,DBMetadata)
execNutleyInstance (NutleyActionObject act) = act >>= execNutleyInstance
execNutleyInstance (NutleyObjInstance inst md) = return (inst,md)

nextInstanceID state = do
  id <- readTVar $ instanceIDCounter state
  writeTVar (instanceIDCounter state) $ id + 1
  return id
                   
freshName state "" = do
  id <- nextInstanceID state
  b <- nameExists state $ "fresh_" ++ (show id)
  if b then freshName state "" else return $ "fresh_" ++ (show id)
freshName _ a = return a


showNutleyObject (NutleySchema (Schema (SC verts' simps) tps)) = result
  where invmp = Map.fromList $ verts'
        verts = map fst verts'
        vertNamed = map (\(i,t) -> (fromJust $ Map.lookup i invmp) ++ " : " ++ (show t)) tps
        simpNamed = map (cim ", "  (fromJust . (flip Map.lookup invmp))) simps
        result = "create schema with\n  vertices =\n  {\n" ++
                 (cim ",\n" ("    "++) vertNamed) ++ "\n  }\n" ++ 
                 "  simplices =\n  {\n" ++ 
                 (cim ",\n" (\x -> "    { "++x++" }") simpNamed) ++ "\n  }"
showNutleyObject (NutleyObjInstance isnt md) = "instance of database " ++ (name md) ++ "\n  " ++ (show isnt)
showNutleyObject (NutleyMap (SchemaMap srcS trgS f))
  = "create map " ++ (showNutleyObject $ NutleySchema srcS) ++ "\n -> \n" ++ (showNutleyObject $ NutleySchema trgS) ++
    "\n with\n  {\n" ++
    (cim ",\n" (\(a,b,Lam _ g) -> "    " ++ (fromJust $ lookup a srcMap) ++ 
                                  " -> " ++ (fromJust $ lookup b trgMap) ++ 
                                  " by " ++ (show g)) f)
    ++ "\n  }"
  where srcMap = schemaVertexNames srcS
        trgMap = schemaVertexNames trgS
showNutleyObject EmptyNutleyObject = "There's no object here"
showNutleyObject (NutleyConnection addr port) = "connect to \"" ++ addr ++ ":" ++ (show port) ++ "\""
showNutleyObject (NutleyActionObject _) = error "show NutleyActionObject not allowed"

newGlobalState = atomically $ do
  no <- newTVar Map.empty
  id <- newTVar 0
  return $ GlobalState { namedObjects = no , instanceIDCounter = id}
                   
clearGlobalState state = atomically $ do
  writeTVar (namedObjects state) Map.empty
  writeTVar (instanceIDCounter state) 0
    
lookupByName :: GlobalState -> Name -> ErrorT STM NutleyObject
lookupByName state name = do
  res <- liftEitherT $ fmap (Map.lookup name) $ readTVar $ namedObjects state
  case res of
    Nothing -> left $ "Object " ++ name ++ " cannot be found"
    (Just a) -> return a

nameExists state name = eitherT (const $ return False) (const $ return True) $ lookupByName state name
  
addNamedObject state name object = flip modifyTVar (Map.insert name object) $ namedObjects state
removeNamedObject state name = flip modifyTVar (Map.delete name) $ namedObjects state

addNamedObjectIfNotExists :: GlobalState -> Name -> NutleyObject -> ErrorT STM ()
addNamedObjectIfNotExists state name object = do
  (hoistEither.(guardEither $ "Object " ++ name ++ " already exists").not) =<< (liftEitherT $ nameExists state name)
  liftEitherT $ addNamedObject state name object

reserveName state name = addNamedObjectIfNotExists state name EmptyNutleyObject