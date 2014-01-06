module HandleUserInput where

import ClientQueries
import GlobalState
import Schema

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent.STM
import Parser

createSchemaSchema (CreateSchema _ verts simps) = do 
  let idMap = Map.fromList $ zip (map (\(TypeDec a _) -> a) verts) [1..]
      tpList = zip [1..] (map (\(TypeDec _ a) -> a) verts)
  guard $ (Map.size idMap) == (length verts)
  simpIds <- forM simps $ mapM (flip Map.lookup idMap)
  return $ NutleySchema (Schema (SC [1.. Map.size idMap] simpIds) tpList) idMap

handleCreateSchema state createSchema@(CreateSchema name _ _) =
  case createSchemaSchema createSchema of
    (Just nutleySchema) -> do
      result <- atomically $ (addNamedObjectIfNotExists state name nutleySchema >> (return $ Nothing))
                `orElse`
                (return $ Just $ "An object by the name " ++ name ++ " already exists")
      case result of
        Nothing -> return $ "Sucessfully added schema " ++ name
        (Just err) -> return err
    Nothing -> return "Improper \'create schema\' Query"

handleShow state (Show name) = do
  obj <- atomically $ (fmap Just (lookupByName state name)) `orElse` (return Nothing)
  case obj of
    (Just a) -> return $ showNutleyObject a
    Nothing  -> return $ "Object " ++ name ++ " not found"

handleUserInput :: GlobalState -> String -> IO String
handleUserInput state str = do
  case parse str of
    Nothing -> return "Parse Error"
    (Just createSchema@(CreateSchema _ _ _)) -> handleCreateSchema state createSchema
    (Just show@(Show _)) -> handleShow state show