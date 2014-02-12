module Main where

import Network.Socket
import Control.Concurrent
import System.IO 
import Control.Monad
import Control.Exception
import System.CPUTime
import Data.Char
import System.Environment

import Parser
import HandleUserInput
import GlobalState
import Utils
import QueryCompile

main :: IO ()
main = do
  args <- getArgs
  let (ad,port) = case args of
        [ad] -> (ad,4242)
        [ad,port] -> (ad,fromIntegral $ read port)
        []   -> ("127.0.0.1",4242)
  addr <- inet_addr ad
  -- create socket
  sock <- socket AF_INET Stream 0
  -- make socket immediately reusable - eases debugging.
  setSocketOption sock ReuseAddr 1
  -- listen on TCP port 4242
  bindSocket sock (SockAddrInet port addr)
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
runConn state (sock, sockaddr) = do
  putStrLn $ "connection opened " ++ (show sockaddr)
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl $ BlockBuffering Nothing
  contents <- hGetContents hdl
  hFlush hdl
  forM (splitContents ';' contents) $ \str -> do 
    evaluate $ length str
    putStrLn $ "Query: " ++ (show $ dropWhile isSpace str)
    response <- handleUserInput state str
    case response of
      (Right "_q") -> hClose hdl
      (Right resp) -> do
        hPutStrLn hdl "\000"
        hPutStrLn hdl resp 
        hPutStrLn hdl "\EOT"
        hFlush hdl
      (Left err) -> do
        hPutStrLn hdl "\001"
        hPutStrLn hdl err
        hPutStrLn hdl "\EOT"
        hFlush hdl
  putStrLn $"connection closed " ++ (show sockaddr)
  hClose hdl

