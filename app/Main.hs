{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import WebServer
import P2P
import Chain

hostName = "localhost"::[Char]
portNumber = 3001::Int
serviceName = "3001":: [Char]



main :: IO ()
main = do 
  chainMV <- newMVar [originBlock]
  (localNode, procId) <- bootstrapP2P hostName serviceName Nothing (return ())
  initP2P localNode chainMV
  httpServer localNode hostName portNumber chainMV 


