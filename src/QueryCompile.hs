module QueryCompile where

import Utils
import Metadata
import Schema
import Name
import System.Plugins.Make
import System.Plugins.Load
import System.Directory

import System.Process
import Data.Char
import qualified Crypto.Hash.SHA256 as SHA
import Data.Serialize

hashQuery (MaterializeQuery md ss) = SHA.finalize $
                                     SHA.update (SHA.update (SHA.update SHA.init (encode "mat"))
                                                 (encode md))
                                     (encode ss)
hashQuery (SectionQuery md ss) = SHA.finalize $
                                 SHA.update (SHA.update (SHA.update SHA.init (encode "sec"))
                                             (encode md))
                                 (encode ss)
hashQuery (InstantiateQuery md) = SHA.finalize $
                                  (SHA.update (SHA.update SHA.init (encode "inst"))
                                   (encode md))
                                  

queryModuleName q = "CQ_" ++ (name q) ++ (hexPrint $ hashQuery q)
queryFileName q = (queryModuleName q) ++ ".hs"

clearPlancache = system $ "rm CQ_*"

makeInclude = makeAll "Include.hs" []

compileQuery :: NutleyQuery -> IO ()
compileQuery q = do
  let (reqs, bod) = codeQuery q
      mname = queryModuleName q
      rnams = map queryModuleName reqs
      fcnts = "module " ++ mname ++ " where\n\n" ++ 
              (concatMap (\r -> "import " ++ r ++ "\n") rnams) ++ "\n" ++
              "import Include\n\n\n" ++ 
              (show bod)
  b <- doesFileExist (mname ++ ".hs")
  if b
    then return ()
    else do
    mapM_ compileQuery reqs
    writeFile (mname ++ ".hs") fcnts
    putStrLn $ "compiling " ++ (name q)
    status <- make (mname ++ ".hs") []
    case status of 
      (MakeSuccess _ _) -> do
        putStrLn $ "sucess: " ++ (mname ++ ".hs")
      (MakeFailure es) -> do
        putStrLn $ "ghc compilation error"
        mapM_ print es
      
