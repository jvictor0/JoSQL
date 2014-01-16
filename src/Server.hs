module Server where

import Network.Socket
import Control.Concurrent
import System.IO 
import Control.Monad
import Control.Exception
import System.CPUTime
import Data.Char
import QueryCompile
import Data.Time.Clock

import HandleUserInput
import GlobalState
import Utils

main :: IO ()
main = do
  -- create socket
  sock <- socket AF_INET Stream 0
  -- make socket immediately reusable - eases debugging.
  setSocketOption sock ReuseAddr 1
  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  -- allow a maximum of 5 outstanding connections
  listen sock 5
  state <- newGlobalState
  mainLoop state sock
 
mainLoop :: GlobalState -> Socket -> IO ()
mainLoop state sock = do
  -- accept one connection and handle it
  conn <- accept sock
  forkIO (runConn state conn)
  mainLoop state sock
               
runConn :: GlobalState -> (Socket, SockAddr) -> IO ()
runConn state (sock, _) = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl $ BlockBuffering Nothing
  contents <- hGetContents hdl
  hPutStr hdl "joSQL> "
  hFlush hdl
  forM (wordsWith ';' contents) $ \str -> do 
    evaluate $ length str
    putStrLn $ "Query: " ++ (dropWhile isSpace str)
    t <-  getCurrentTime
    response <- handleUserInput state str
    case response of
      "_q" -> hClose hdl
      _   -> return ()
    evaluate $ length response
    t' <- getCurrentTime
    hPutStrLn hdl $ "Query completed in " ++ (show $ (diffUTCTime t' t))
    hPutStrLn hdl response
    hPutStr hdl "joSQL> "
    hFlush hdl
  putStrLn "user disconnected"
  hClose hdl

