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
import Data.Either
import Control.Concurrent.STM
import Parser
import Control.Monad.Trans.Either

createToObject :: GlobalState -> CreateQuery -> ErrorT STM NutleyObject
createToObject state (CreateSchema verts simps) = do 
  let idMap = Map.fromList $ zip (map (\(TypeDec a _) -> a) verts) [1..]
      tpList = zip [1..] (map (\(TypeDec _ a) -> a) verts)
  hoistEither $ guardEither "Duplicate vertex name in create schema" $ (Map.size idMap) == (length verts)
  simpIds <- hoistEither $ forM simps $ mapM ((maybeToEither "simplex not in schema").(flip Map.lookup idMap))
  return $ NutleySchema (Schema (SC (zip [1.. Map.size idMap] $ map (\(TypeDec a _) -> a) verts) simpIds) tpList)
createToObject state (InstantiateSchema schq simplex dat) = do 
  sch <- schemaQuerySchema state schq
  id <- liftEitherT $ nextInstanceID state
  case mapM (\s -> schemaLookupVertex s sch) simplex of
    (Just simp) -> do 
      hoistEither $ guardEither "Cannot instantiate schema at simplex not in schema" $ containsSimplex (fullSubSchema sch) simp
      let md = simpleRecord (name schq) sch simp
      return $ NutleyActionObject $ do 
        inst <- case dat of 
          (ExplicitTuples tps) -> do
            executeInstantiateFromStrings (InstantiateQuery md) id tps
          (LoadCSV filepath) -> do
            dfe <- liftEitherT $ doesFileExist filepath
            if not dfe
              then left $ "Cannot find file " ++ filepath
              else do
              dats <- liftEitherT $ fmap ((map (sepBySkipQuotes (==','))).lines) $ readFile filepath
              let tups = map (map (\x -> if x == "null" then Nothing else Just x)) dats
              executeInstantiateFromStrings (InstantiateQuery md) id tups
        return $ NutleyObjInstance inst md
    Nothing -> left "Cannot instantiate schema at vertices not in schema"
createToObject state (FilterQuery inner fn) = do
  instObj <- instanceQueryInstance state inner
  hoistEither $ withNutleyInstance instObj $ \(inst,md) -> do
    (md,params) <- subInstance fn md
    return $ NutleyObjInstance (SimpleSubInstance params inst) md
createToObject state (UnionQuery us) = do
  instances <- mapM (instanceQueryInstance state) us
  hoistEither $ withNutleyInstances instances $ \inners -> do
    let (db,ni) = coLimitOne inners 
    verifyEither db -- wait.. why are we verifying this db?
    return $ NutleyObjInstance ni db
createToObject state (CreateMap srcQ trgQ defs) = do
  schSrc <- schemaQuerySchema state srcQ
  schTrg <- schemaQuerySchema state trgQ
  case mapM (\(a,b,f) -> (Just (,,)) `ap` (schemaLookupVertex a schSrc) `ap` (schemaLookupVertex b schTrg) `ap` (Just f)) defs of
    (Just defs) -> return $ NutleyMap $ SchemaMap schSrc schTrg defs
    Nothing -> left $ "Mapped vertex not in schema"
createToObject state (FunctorQuery t mapQuery instanceQuery) = do    
  f <- mapQueryMap state mapQuery
  instObj <- instanceQueryInstance state instanceQuery
  hoistEither $ withNutleyInstance instObj $ \(inst,md) -> do
    case t of
      ShriekFunctor -> fmap (NutleyObjInstance $ Shriek inst) $ verifyEither $ shriek f md
      DirectImageFunctor -> fmap (NutleyObjInstance $ DirectImage inst) $ verifyEither $ directImage f md
      InverseImageFunctor -> do
        (invmd,params) <- inverseImage f md
        return $ NutleyObjInstance (InverseImage params inst) invmd

withNutleyInstance :: NutleyObject -> ((NutleyInstance,DBMetadata) -> Error NutleyObject) -> Error NutleyObject
withNutleyInstance (NutleyObjInstance inst md) f = f (inst,md)
withNutleyInstance (NutleyActionObject ioAction) f = return $ NutleyActionObject $ do
  obj <- ioAction
  hoistEither $ withNutleyInstance obj f
withNutleyInstances :: [NutleyObject] -> ([(DBMetadata,NutleyInstance)] -> Error NutleyObject) -> Error NutleyObject
withNutleyInstances instances f 
  | all (not.isActionObject) instances = f $ map (\(NutleyObjInstance inst md) -> (md,inst)) instances
  | otherwise = do
    return $ NutleyActionObject $ do
      objs <- forM instances $ \i -> case i of
        (NutleyObjInstance inst md) -> return i
        (NutleyActionObject ioAction) -> ioAction
      hoistEither $ withNutleyInstances objs f

handleLetName :: GlobalState -> ClientQuery -> IO String
handleLetName state (LetQuery name create) = eitherT return return $ do
  join $ mapEitherT atomically $ do
    obj <- createToObject state create 
    case obj of
      (NutleyActionObject ioObj) -> (>>) (reserveName state name) $ return $ do
        result <- liftEitherT $ runEitherT ioObj
        case result of
          (Left err) -> (liftEitherT $ atomically $ removeNamedObject state name) >> left err
          (Right nobj@(NutleyObjInstance _ _)) -> liftEitherT $  atomically $ addNamedObject state name nobj
          _ -> error "something has gone wrong"
      _ -> do
        addNamedObjectIfNotExists state name obj 
        return $ return ()
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
instanceQueryInstance state (CreateInstance createQuery) = createToObject state createQuery
  

handleSelect :: GlobalState -> ClientQuery -> IO String
handleSelect state (SelectQuery simpNamed instanceQuery) = eitherT return return $ do
  instanceObj <- mapEitherT atomically $ instanceQueryInstance state instanceQuery 
  (inst,md) <- execNutleyInstance instanceObj -- THIS CAN LEAK MEMORY IF instanceObj IS AN INSTANTIATE QUERY
  ss <- case simplicesFromNames md simpNamed of
    Nothing -> left "Simplex not in instance's schema"
    (Just simps) -> do
      right (SubSchema simps $ dbSchema md)
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
    