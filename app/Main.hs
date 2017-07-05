{-# LANGUAGE OverloadedStrings #-}
module Main where

import WebServer
import P2P
import Chain
import Logging
import Data.Maybe                       (isNothing)
import System.Environment               (getArgs)
import Control.Concurrent.MVar


data Config = Config {
  hostname :: String,
  httpPort :: String,
  p2pPort  :: String,
  seedHost :: Maybe String
}


main :: IO ()
main = do 
  config <- getArgs >>= \a -> case a of
        [host, httpPort, p2pPort] -> return $ Config host httpPort p2pPort Nothing 
        [host, httpPort, p2pPort, seedHost] -> return $ Config host httpPort p2pPort ( Just seedHost)
        _ -> fail "Usage:\n\n$ protoChain host httpPort p2pPort \n\n\n" 

  let host      = hostname config
      port_Http = httpPort config
      port_P2P  = p2pPort config
      seed      = seedHost config

  _ <- initLogger port_P2P

  liftDebug $ "Starting BlockChain Application......"
  chainMV <- newMVar [originBlock]
  (localNode, procId) <- bootstrapP2P host port_P2P seed (return ())
  liftDebug $ "Bootstrapping P2P Done..."
  
  initP2P localNode chainMV $ isNothing seed 
  liftDebug $ "Initialising P2P Done..."

  httpServer localNode host (read port_Http) chainMV 
  liftDebug $ "BlockChain Application is initialised and ready."
  

