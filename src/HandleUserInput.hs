module HandleUserInput where

import ClientQueries
import GlobalState
import Schema
import Utils
import SimpleRecord
import SimpleSubInstance
import CoLimit
import Types
import Execute
import Metadata
import NutleyQuery
import QueryCompile
import NutleyInstance
import Name
import Verify
import Shriek
import DirectImage
import System.Directory
import InverseImage

import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent.STM
import Parser
import Control.Monad.Trans.Either

-- TODO: this should NOT be IO, this should return the IO action
createToObject :: GlobalState -> CreateQuery -> ErrorT IO NutleyObject 
createToObject state (CreateSchema verts simps) = do 
  let idMap = Map.fromList $ zip (map (\(TypeDec a _) -> a) verts) [1..]
      tpList = zip [1..] (map (\(TypeDec _ a) -> a) verts)
  hoistEither $ guardEither "Duplicate vertex name in create schema" $ (Map.size idMap) == (length verts)
  simpIds <- hoistEither $ forM simps $ mapM ((maybeToEither "simplex not in schema").(flip Map.lookup idMap))
  return $ NutleySchema (Schema (SC (zip [1.. Map.size idMap] $ map (\(TypeDec a _) -> a) verts) simpIds) tpList)
createToObject state (InstantiateSchema schq simplex dat) = do 
  sch <- mapEitherT atomically $ schemaQuerySchema state schq
  id <- mapEitherT atomically $ liftEitherT $ nextInstanceID state
  case mapM (\s -> schemaLookupVertex s sch) simplex of
    (Just simp) -> do 
      let md = simpleRecord (name schq) sch simp
      inst <- case dat of 
        (ExplicitTuples tps) -> do
          executeInstantiateFromStrings (InstantiateQuery md) id tps
        (LoadCSV filepath) -> do
          dfe <- liftEitherT $ doesFileExist filepath
          if not dfe
            then left $ "Cannot find file " ++ filepath
            else do
            dats <- liftEitherT $ fmap ((map (sepBy (==','))).lines) $ readFile filepath
            let tups = map (map (\x -> if x == "null" then Nothing else Just x)) dats
            executeInstantiateFromStrings (InstantiateQuery md) id tups
      return $ NutleyObjInstance inst md
    Nothing -> left "Cannot instantiate schema, simplex not in schema"
createToObject state (FilterQuery inner fn) = do
  (NutleyObjInstance inst md) <- mapEitherT atomically $ instanceQueryInstance state inner
  (md,params) <- EitherT $ return $ subInstance fn md
  return $ NutleyObjInstance (SimpleSubInstance params inst) md
createToObject state (UnionQuery us) = do
  inners <- fmap (map (\(NutleyObjInstance inst md) -> (md,inst))) $ 
            mapEitherT atomically $ mapM (instanceQueryInstance state) us
  let (db,ni) = coLimitOne inners 
  hoistEither $ verifyEither db
  return $ NutleyObjInstance ni db
createToObject state (CreateMap srcQ trgQ defs) = do
  schSrc <- mapEitherT atomically $ schemaQuerySchema state srcQ
  schTrg <- mapEitherT atomically $ schemaQuerySchema state trgQ
  case mapM (\(a,b,f) -> (Just (,,)) `ap` (schemaLookupVertex a schSrc) `ap` (schemaLookupVertex b schTrg) `ap` (Just f)) defs of
    (Just defs) -> return $ NutleyMap $ SchemaMap schSrc schTrg defs
    Nothing -> left $ "Mapped vertex not in schema"
createToObject state (FunctorQuery t mapQuery instanceQuery) = do    
  f <- mapEitherT atomically $ mapQueryMap state mapQuery
  (NutleyObjInstance inst md) <- mapEitherT atomically $ instanceQueryInstance state instanceQuery
  case t of
    ShriekFunctor -> fmap (NutleyObjInstance $ Shriek inst) $ hoistEither $ verifyEither $ shriek f md
    DirectImageFunctor -> fmap (NutleyObjInstance $ DirectImage inst) $ hoistEither $ verifyEither $ directImage f md
    InverseImageFunctor -> do
      (invmd,params) <- hoistEither $ inverseImage f md
      return $ NutleyObjInstance (InverseImage params inst) invmd

handleLetName state (LetQuery name create) = eitherT return return $ do
  nutleyObject <- createToObject state create 
  mapEitherT atomically $ addNamedObjectIfNotExists state name nutleyObject 
  return $ "Added object " ++ name
  
handleShow state (ShowQuery name) = eitherT return (return.showNutleyObject)
                                    $ mapEitherT atomically $ lookupByName state name

schemaQuerySchema :: GlobalState -> SchemaQuery -> ErrorT STM Schema
schemaQuerySchema state (NamedSchema name) = do 
  (NutleySchema s) <- lookupByName state name 
  return s

mapQueryMap :: GlobalState -> MapQuery -> ErrorT STM SchemaMap
mapQueryMap state (NamedMap name) = do 
  (NutleyMap f) <- lookupByName state name 
  return f

instanceQueryInstance :: GlobalState -> InstanceQuery -> ErrorT STM NutleyObject
instanceQueryInstance state (NamedInstance name) = lookupByName state name

handleSelect :: GlobalState -> ClientQuery -> IO String
handleSelect state (SelectQuery simpNamed instanceQuery) = eitherT return return $ do
  (NutleyObjInstance inst md) <- mapEitherT atomically $ instanceQueryInstance state instanceQuery 
  ss <- case simplicesFromNames md simpNamed of
    Nothing -> left "Simplex not in instance's schema"
    (Just simps) -> right (SubSchema simps $ dbSchema md)
  executeSectionString (SectionQuery md ss) inst

handleUserInput :: GlobalState -> String -> IO String
handleUserInput state str = do
  case parse str of
    Nothing -> return "Parse Error"
    (Just letq@(LetQuery _ _)) -> handleLetName state letq
    (Just show@(ShowQuery _)) -> handleShow state show
    (Just sele@(SelectQuery _ _)) -> handleSelect state sele
    (Just ClearCache) -> clearPlancache >> (return "")
    (Just ClearData) -> clearData >> (clearGlobalState state) >> (return "")
    (Just Quit) -> return "_q"
    (Just KILLServer) -> return "_k"
    