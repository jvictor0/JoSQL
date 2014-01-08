module HandleUserInput where

import ClientQueries
import GlobalState
import Schema
import Utils
import SimpleRecord
import Types
import Execute
import Metadata
import NutleyQuery

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent.STM
import Parser
import Control.Monad.Trans.Either


createToObject :: GlobalState -> Name -> CreateQuery -> ErrorT IO NutleyObject 
createToObject state name (CreateSchema verts simps) = do 
  let idMap = Map.fromList $ zip (map (\(TypeDec a _) -> a) verts) [1..]
      tpList = zip [1..] (map (\(TypeDec _ a) -> a) verts)
  hoistEither $ guardEither "Duplicate vertex name in create schema" $ (Map.size idMap) == (length verts)
  simpIds <- hoistEither $ forM simps $ mapM ((maybeToEither "simplex not in schema").(flip Map.lookup idMap))
  return $ NutleySchema (Schema (SC [1.. Map.size idMap] simpIds) tpList) idMap
createToObject state name (InstantiateSchema sch simplex dat) = do 
  (NutleySchema sch cols) <- mapEitherT atomically $ schemaQuerySchema state sch
  id <- mapEitherT atomically $ liftEitherT $ nextInstanceID state
  case mapM (flip Map.lookup cols) simplex of
    (Just simp) -> do 
      let md = simpleRecord name sch simp
      inst <- case dat of 
        (ExplicitTuples tps) -> EitherT $ executeInstantiateFromStrings (InstantiateQuery md) id tps
      return $ NutleyObjInstance inst md
    Nothing -> left "Cannot instantiate schema, simplex not in schema"
      
handleLetName state (LetQuery name create) = eitherT return return $ do
  nutleyObject <- createToObject state name create 
  mapEitherT atomically $ addNamedObjectIfNotExists state name nutleyObject 
  return $ "Added object " ++ name
  
handleShow state (Show name) = eitherT return (return.showNutleyObject)
                               $ mapEitherT atomically $ lookupByName state name

schemaQuerySchema :: GlobalState -> SchemaQuery -> ErrorT STM NutleyObject
schemaQuerySchema state (NamedSchema name) = lookupByName state name 
-- schemaQuerySchema state _ = retry

handleUserInput :: GlobalState -> String -> IO String
handleUserInput state str = do
  case parse str of
    Nothing -> return "Parse Error"
    (Just letq@(LetQuery _ _)) -> handleLetName state letq
    (Just show@(Show _)) -> handleShow state show