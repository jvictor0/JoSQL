module Execute where

import Utils
import Metadata
import Schema
import Name
import QueryCompile
import Types
import NutleyInstance
import Include
import SerializeCode
import NutleyQuery

import System.Plugins.Make
import System.Plugins.Load
import System.Directory
import System.Process
import Data.Char
import Control.Monad.Trans.Either

{-
instantiateWithType :: (InstanceID -> tups -> IO NutleyInstance) -> tups -> 
                       (InstanceID -> tups -> IO NutleyInstance)
instantiateWithType f _ = f

sectionWithType :: (NutleyInstance -> IO tups) -> tups -> 
                   (NutleyInstance -> IO tups)
sectionWithType f _ = f
-}
{-
executeInstantiate :: NutleyQuery -> InstanceID -> tups -> ErrorT IO NutleyInstance
executeInstantiate q instID inData = do
  modname <- compileQuery q
  ls <- liftEitherT $ load (modname ++ ".o") ["."] [] (name q)
  case ls of
    (LoadSuccess _ f) -> (f`instantiateWithType`inData) instID inData
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> (right $ cim "\n" id errs)
-}    


executeInstantiateFromStrings :: NutleyQuery -> InstanceID -> [[Maybe String]] -> ErrorT IO NutleyInstance
executeInstantiateFromStrings q instID inData = do
  modname <- compileQuery q
  ls <- liftEitherT $ load (modname ++ ".o") ["."] [] (stringsInstantiateName q)
  case ls of
    (LoadSuccess _ f) -> (EitherT $ f instID inData) 
    (LoadFailure errs) -> (mapM_ (liftEitherT.putStrLn) errs) >> (left $ cim "\n" id errs)

executeInstantiateSelect :: NutleyQuery -> InstanceID -> NutleyInstance -> ErrorT IO NutleyInstance
executeInstantiateSelect q instID fromInstance = do
  modname <- compileQuery q
  ls <- liftEitherT $ load (modname ++ ".o") ["."] [] (name q)
  case ls of
    (LoadSuccess _ f) -> (liftEitherT $ f instID fromInstance) 
    (LoadFailure errs) -> (mapM_ (liftEitherT.putStrLn) errs) >> (left $ cim "\n" id errs)


{-
executeInstantiateSerialize :: NutleyQuery -> InstanceID -> LazyByteString -> IO ByteString
executeInstantiateSerialize q instID inData = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] [] (serializedName q)
  case ls of
    (LoadSuccess _ f) -> f instID inData -- type is completely inferrable, cool!
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""
    
    
executeSection :: tups -> NutleyQuery -> NutleyInstance -> IO tups
executeSection tps q instID = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] []  (name q)
  case ls of
    (LoadSuccess m f) -> (f`sectionWithType`tps) instID
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""

executeSectionSerialize :: NutleyQuery -> NutleyInstance -> IO LazyByteString
executeSectionSerialize q instID = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] []  (serializedName q)
  case ls of
    (LoadSuccess m f) -> f instID
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""
-}

executeSectionString :: NutleyQuery -> NutleyInstance -> ErrorT IO String
executeSectionString q instID = do
  modname <- compileQuery q
  ls <- liftEitherT $ load (modname ++ ".o") ["."] []  (stringResultName q)
  case ls of
    (LoadSuccess m f) -> liftEitherT $ f instID
    (LoadFailure errs) -> (mapM_ (liftEitherT.putStrLn) errs) >> (left $ cim "\n" id errs)

