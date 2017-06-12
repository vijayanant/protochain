{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import WebServer
import P2P
import Chain

import Control.Monad.Trans
import System.Log.Formatter
import System.Log.Handler               (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import System.IO                        (stdout)
hostName = "localhost"::[Char]
portNumber = 9090::Int
serviceName = "9001":: [Char]


-- sets up a logger to stdout as well as legion${port}.log
initLogger :: String -> IO ()
initLogger serviceName = 
  let logPriority = DEBUG
      format lh = return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  in
    streamHandler stdout logPriority >>= format >>= \s ->
      fileHandler ("proto-chain" ++ serviceName ++ ".log") logPriority >>= format >>= \h ->
        updateGlobalLogger rootLoggerName $ setLevel logPriority . setHandlers [s, h]

liftDebug :: (MonadIO m) => String -> m ()
liftDebug str = liftIO $ debugM "proto-chain" (show str)

main :: IO ()
main = do 
  _ <- initLogger serviceName

  liftDebug $ "Starting BlockChain Application......"
  chainMV <- newMVar [originBlock]
  (localNode, procId) <- bootstrapP2P hostName serviceName Nothing (return ())
  liftDebug $ "Bootstrapping P2P Done..."
  
  httpServer localNode hostName portNumber chainMV 
  liftDebug $ "BlockChain Application is initialised and ready."
  
  initP2P localNode chainMV
  liftDebug $ "Initialising P2P Done..."


