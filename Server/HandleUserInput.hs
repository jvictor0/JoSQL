module Server.HandleUserInput where

import Server.ClientQueries
import Server.GlobalState
import Data.Schema
import Utils.Utils
import CodeGen.Metadata.SimpleRecord
import CodeGen.Metadata.SimpleSubInstance
import CodeGen.Metadata.CoLimit
import Data.Types
import Server.Execute
import CodeGen.Metadata.Metadata
import CodeGen.NutleyQuery
import Server.QueryCompile
import Server.NutleyInstance
import Data.Name
import Utils.Verify
import CodeGen.Metadata.Shriek
import CodeGen.Metadata.DirectImage
import System.Directory
import CodeGen.Metadata.InverseImage

import qualified Data.Map as Map
import Control.Monad
import Data.Either
import Control.Concurrent.STM
import Server.Parser
import Control.Monad.Trans.Either
import Network.Socket
import Network
import System.IO

createToObject :: GlobalState -> CreateQuery -> ErrorT STM NutleyObject
createToObject state (NamedObject name) = lookupByName state name
createToObject state (SchemaQuery sch) = fmap NutleySchema $ schemaQuerySchema state sch
createToObject state cs@(CreateSchema verts simps) = fmap NutleySchema $ hoistEither $ createSchemaToSchema cs
createToObject state (InstantiateSchema schq simplex dat) = do 
  sch <- schemaQuerySchema state schq
  id <- liftEitherT $ nextInstanceID state
  case mapM (\s -> schemaLookupVertex s sch) simplex of
    (Just simp) -> do 
      hoistEither $ guardEither "Cannot instantiate schema at simplex not in schema" $ containsSimplex (fullSubSchema sch) simp
      let md = simpleRecord (name schq) sch simp
      case dat of 
        (ExplicitTuples tps) -> return $ NutleyActionObject $ do 
          fmap (flip NutleyObjInstance md) $ executeInstantiateFromStrings (InstantiateQuery md) id tps
        (LoadCSV filepath) -> return $ NutleyActionObject $ do 
          dfe <- liftEitherT $ doesFileExist filepath
          if not dfe
            then left $ "Cannot find file " ++ filepath
            else do
            dats <- liftEitherT $ fmap ((map (sepBySkipQuotes (==','))).lines) $ readFile filepath
            let tups = map (map (\x -> if x == "null" then Nothing else Just x)) dats
            fmap (flip NutleyObjInstance md) $ executeInstantiateFromStrings (InstantiateQuery md) id tups
        (SelectData (SelectQuery simpNamed from)) -> do
          instanceObj <- instanceQueryInstance state from
          ss <- case simplicesFromNames md simpNamed of
            Nothing -> left "Simplex not in instance's schema"
            (Just simps) -> do
              right (SubSchema simps $ dbSchema md)
          return $ NutleyActionObject $ do 
            (inst,mdfrom) <- execNutleyInstance instanceObj
            fmap (flip NutleyObjInstance md) $ executeInstantiateSelect (InstantiateSelectQuery md mdfrom ss) id inst
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
createToObject state (ConnectQuery addr port) =
  return $ NutleyConnection addr $ PortNumber $ fromIntegral port
  
createSchemaToSchema :: CreateQuery -> Error Schema
createSchemaToSchema (CreateSchema verts simps) = do 
  let idMap = Map.fromList $ zip (map (\(TypeDec a _) -> a) verts) [1..]
      tpList = zip [1..] (map (\(TypeDec _ a) -> a) verts)
  guardEither "Duplicate vertex name in create schema" $ (Map.size idMap) == (length verts)
  simpIds <- forM simps $ mapM ((maybeToEither "simplex not in schema").(flip Map.lookup idMap))
  return $ Schema (SC (zip [1.. Map.size idMap] $ map (\(TypeDec a _) -> a) verts) simpIds) tpList
  
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



handleLetName :: GlobalState -> ClientQuery -> IO (Error String)
handleLetName state (LetQuery name' (OnConnection c query)) = runEitherT $ do
  (name,NutleyConnection addr port) <- mapEitherT atomically $ do
    name <- liftEitherT $ freshName state name'
    conn <- connectQueryConnect state c
    reserveName state name
    return (name,conn)
  hdl <- tryErrorT $ connectTo addr port
  tryErrorT $ hPutStrLn hdl $ "let " ++ name ++ " = " ++ query ++ ";"
  (e:trans) <- hGetTransmission hdl
  hoistEither $ guardEither ("node error: " ++ concat trans) $ e == "\000"
  tryErrorT $ hPutStrLn hdl $ "show (schema " ++ name ++ ")"
  (e2:createSchemaQuery:rst) <- hGetTransmission hdl
  if e == "\000"
    then do
    hoistEither $ guardEither "something has gone wrong" $ null rst  
    let (Just schQ) = (\(Node TopLevel lt) -> parseCreateSchema lt) =<< (lexTree createSchemaQuery)
    sch <- hoistEither $ createSchemaToSchema schQ
    let nobj = NutleyObjInstance (RemoteInstance addr port) $ error "no remote"-- (remoteInstance name sch)
    liftEitherT $ atomically $ addNamedObject state name nobj
    return $ "Added object " ++ name
    else do
    tryErrorT $ hPutStrLn hdl $ "show " ++ name ++ ";"
    (e2:createQuery:rst) <- hGetTransmission hdl  
    hoistEither $ guardEither ("node error: " ++ concat trans) $ e2 == "\000"
    hoistEither $ guardEither "something went wrong" $ null rst
    res <- liftEitherT $ handleUserInput state $ "let " ++ name ++ " = " ++ createQuery
    case res of
     (Left err) -> (liftEitherT $ atomically $ removeNamedObject state name) >> left err
     (Right note) -> return $ "Added object " ++ name  
handleLetName state (LetQuery name' create) = runEitherT $ do
  join $ mapEitherT atomically $ do
    name <- liftEitherT $ freshName state name'
    obj <- createToObject state create 
    case obj of
      (NutleyActionObject ioObj) -> (>>) (reserveName state name) $ return $ do
        result <- liftEitherT $ runEitherT ioObj
        case result of
          (Left err) -> (liftEitherT $ atomically $ removeNamedObject state name) >> left err
          (Right nobj) -> do
            hoistEither $ guardEither "something has gone wrong" $ not $ isActionObject nobj
            liftEitherT $ atomically $ addNamedObject state name nobj
            return $ "Added object " ++ name
      _ -> do
        addNamedObjectIfNotExists state name obj 
        return $ return $ "Added object " ++ name

  
handleShow state (ShowQuery create) = do
  res <- runEitherT $ do
    obj <- mapEitherT atomically $ createToObject state create
    case obj of
      (NutleyActionObject a) -> a
      _ -> return obj
  return $ fmap showNutleyObject res

schemaQuerySchema :: GlobalState -> SchemaQuery -> ErrorT STM Schema
schemaQuerySchema state (NamedSchema name) = do 
  (NutleySchema s) <- lookupByName state name 
  return s
schemaQuerySchema state (SchemaOf instanceQuery) = instanceQuerySchema state instanceQuery


mapQueryMap :: GlobalState -> MapQuery -> ErrorT STM SchemaMap
mapQueryMap state (NamedMap name) = do 
  (NutleyMap f) <- lookupByName state name 
  return f

connectQueryConnect :: GlobalState -> ConnectQuery -> ErrorT STM NutleyObject
connectQueryConnect state (AddressedConnect addr port) = return $ NutleyConnection addr $ PortNumber $ fromIntegral port
connectQueryConnect state (NamedConnect name) = lookupByName state name

instanceQueryInstance :: GlobalState -> InstanceQuery -> ErrorT STM NutleyObject
instanceQueryInstance state (NamedInstance name) = lookupByName state name
instanceQueryInstance state (CreateInstance createQuery) = createToObject state createQuery
  
instanceQuerySchema state (NamedInstance name) = do
  res <- lookupByName state name
  case res of
    (NutleyObjInstance _ md) -> return $ dbSchema md
    _ -> left $ "object " ++ name ++ " has no schema"
instanceQuerySchema state (CreateInstance (InstantiateSchema schq _ _)) = schemaQuerySchema state schq
instanceQuerySchema state (CreateInstance (FilterQuery inst _)) = instanceQuerySchema state inst
instanceQuerySchema state (CreateInstance (FunctorQuery ShriekFunctor f _)) = mapQueryCoDomainSchema state f
instanceQuerySchema state (CreateInstance (FunctorQuery DirectImageFunctor f _)) = mapQueryCoDomainSchema state f
instanceQuerySchema state (CreateInstance (FunctorQuery InverseImageFunctor f _)) = mapQueryDomainSchema state f
instanceQuerySchema state (CreateInstance (UnionQuery (a:as))) = instanceQuerySchema state a
instanceQuerySchema state (CreateInstance (UnionQuery [])) = return emptySchema
instanceQuerySchema state _ = left "cannot take schema of that object"
  
mapQueryCoDomainSchema state mapQuery = do
  (SchemaMap _ cdm _) <- mapQueryMap state mapQuery
  return cdm
  
mapQueryDomainSchema state mapQuery = do
  (SchemaMap dom _ _) <- mapQueryMap state mapQuery
  return dom


handleSelect :: GlobalState -> ClientQuery -> IO (Error String)
handleSelect state (SelectQuery simpNamed instanceQuery) = runEitherT $ do
  instanceObj <- mapEitherT atomically $ instanceQueryInstance state instanceQuery 
  (inst,md) <- execNutleyInstance instanceObj -- THIS CAN LEAK MEMORY IF instanceObj IS AN INSTANTIATE QUERY
  ss <- case simplicesFromNames md simpNamed of
    Nothing -> left "Simplex not in instance's schema"
    (Just simps) -> do
      right (SubSchema simps $ dbSchema md)
  executeSectionString (SectionQuery md ss) inst

handleUserInput :: GlobalState -> String -> IO (Error String)
handleUserInput state str = do
  case parse str of
    Nothing -> return $ Left "Parse Error"
    (Just letq@(LetQuery _ _)) -> handleLetName state letq
    (Just show@(ShowQuery _)) -> handleShow state show
    (Just sele@(SelectQuery _ _)) -> handleSelect state sele
    (Just ClearCache) -> clearPlancache >> (return $ Right "")
    (Just ClearData) -> clearData >> (clearGlobalState state) >> (return $ Right "")
    (Just Quit) -> return $ Right "_q"
    