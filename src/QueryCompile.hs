module QueryCompile where

import Utils
import Metadata
import Schema
import Name
import System.Plugins.Make
import System.Plugins.Load
import System.Directory
import SerializeCode
import NutleyQuery
import Types

import System.Process
import Data.Char
import qualified Crypto.Hash.SHA256 as SHA
import Data.Serialize
import qualified Data.ByteString as BS
import Control.Monad.Trans.Either


hashQuery (MaterializeQuery md ss) = SHA.finalize $
                                     SHA.update (SHA.update (SHA.update SHA.init (encode "mat"))
                                                 (dbHashCode md))
                                     (encode ss)
hashQuery (SectionQuery md ss) = SHA.finalize $
                                 SHA.update (SHA.update (SHA.update SHA.init (encode "sec"))
                                             (dbHashCode md))
                                 (encode ss)
hashQuery (InstantiateQuery md) = SHA.finalize $
                                  (SHA.update (SHA.update SHA.init (encode "inst"))
                                   (dbHashCode md))
                                 

queryModuleName q = "CQ_" ++ (name q) ++ (hexPrint $ hashQuery q)
queryFileName q = (queryModuleName q) ++ ".hs"

clearPlancache = system $ "rm CQ_*"
clearData = system $ "rm *.seg"
clearAll = clearData >> clearPlancache

clearCompiled = system "rm *.o *.hi"

compileQuery :: NutleyQuery -> ErrorT IO String
compileQuery q = do
  let (reqs, bod) = codeQuery q
      mname = queryModuleName q
      rnams = map queryModuleName $ map snd reqs
      fcnts = "module " ++ mname ++ " where\n\n" ++ 
              (concatMap (\(qn,r) -> "import qualified " ++ r ++ " as " ++ qn ++ "\n") $ zip (map fst reqs) rnams) ++ "\n" ++
              "import Include\n\n\n" ++ 
              (show bod) ++ "\n\n\n" ++ 
              (cim "\n\n\n" show $ serializeCode q)
  b <- liftEitherT $ doesFileExist (mname ++ ".hs")
  if b
    then return mname
    else do
    liftEitherT $ putStrLn $ "compiling " ++ (name q)
    mapM_ compileQuery $ map snd reqs
    liftEitherT $ writeFile (mname ++ ".hs") fcnts
    status <- liftEitherT $ make (mname ++ ".hs") []
    case status of 
      (MakeSuccess _ _) -> do
        liftEitherT $ putStrLn $ "sucess: " ++ (mname ++ ".hs")
        return mname
      (MakeFailure es) -> do
        liftEitherT $ putStrLn $ "ghc compilation error"
        liftEitherT $ mapM_ putStrLn es
        left $ "ghc compiler error, check functions from query"
      
