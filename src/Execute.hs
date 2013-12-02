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

import System.Plugins.Make
import System.Plugins.Load
import System.Directory
import System.Process
import Data.Char
import Data.Typeable

instantiateWithType :: (InstanceID -> tups -> IO nutleyInstance) -> (nutleyInstance,tups) -> 
                       (InstanceID -> tups -> IO nutleyInstance)
instantiateWithType f _ = f

sectionWithType :: (nutleyInstance -> IO tups) -> (nutleyInstance,tups) -> 
                   (nutleyInstance -> IO tups)
sectionWithType f _ = f

executeInstantiate :: nutleyInstance -> NutleyQuery -> InstanceID -> tups -> IO nutleyInstance
executeInstantiate ni q instID inData = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] [] (name q)
  case ls of
    (LoadSuccess _ f) -> (f`instantiateWithType`(ni,inData)) instID inData
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""
    
executeInstantiateSerialize :: NutleyQuery -> InstanceID -> LazyByteString -> IO ByteString
executeInstantiateSerialize q instID inData = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] [] (serializedName q)
  case ls of
    (LoadSuccess _ f) -> f instID inData -- type is completely inferrable, cool!
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""
    
    
executeSection :: tups -> NutleyQuery -> nutleyInstance -> IO tups
executeSection tps q instID = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] []  (name q)
  case ls of
    (LoadSuccess m f) -> (f`sectionWithType`(instID,tps)) instID
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""

executeSectionSerialize :: NutleyQuery -> ByteString -> IO LazyByteString
executeSectionSerialize q instID = do
  modname <- compileQuery q
  ls <- load (modname ++ ".o") ["."] []  (serializedName q)
  case ls of
    (LoadSuccess m f) -> f instID
    (LoadFailure errs) -> (mapM_ putStrLn errs) >> error ""

