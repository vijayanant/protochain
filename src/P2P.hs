{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module P2P where

import Protolude                                  as P
import qualified Control.Distributed.Backend.P2P  as P2P
import qualified Data.Serialize                   as S
import qualified Data.Binary                      as B

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Messaging
import Chain

instance B.Binary Msg
instance B.Binary Block

p2pServiceName = "BlockChain-P2PService"


bootstrapP2P  port bootstrapNode = P2P.bootstrapNonBlocking "localhost" port (maybeToList $ P2P.makeNodeId `fmap` bootstrapNode) initRemoteTable

initP2P portNumber = do 
  (localNode, procId) <- bootstrapP2P portNumber Nothing (return ())
  chainMV <- newMVar [originBlock]
  -- wait for messages to come in from the p2p network and respond to them
  runProcess localNode  $ do 
    getSelfPid >>= register p2pServiceName
    liftIO $ threadDelay 1000000
    forever $ do
      message <- expect :: Process Msg
      mRespMessage <- messageHandler chainMV message
      case mRespMessage of
        Nothing -> return ()
        Just msg -> sendMessage localNode msg

sendMessage :: MonadIO m => LocalNode -> Msg -> m ()
sendMessage localNode message  = liftIO $ runProcess localNode $ do
  P2P.nsendPeers p2pServiceName $ message
