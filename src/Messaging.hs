{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging where

import Protolude                                  as P
import Control.Distributed.Process
import Control.Distributed.Process.Node
import qualified Control.Distributed.Backend.P2P  as P2P
import qualified Data.Serialize                   as S
import qualified Data.Binary                      as B

import Chain

data Msg 
  = QueryLatestBlock
  | QueryBlockChain
  | RespBlockChain BlockChain
  | RespLatestBlock Block   
  deriving (Generic)

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
      messageHandler localNode chainMV message


messageHandler :: MonadIO m => LocalNode -> MVar BlockChain -> Msg -> m ()
messageHandler localNode chain QueryLatestBlock =  do
  mBlock <- liftIO $  getLatestBlock chain
  case mBlock of
    Just block -> sendMessage localNode $ RespLatestBlock block
    Nothing -> return ()
messageHandler localNode chain QueryBlockChain = do 
  mChain <- liftIO $ readMVar chain
  sendMessage localNode $ RespBlockChain mChain
messageHandler localNode chain ( RespLatestBlock block ) = do 
      mMessage <- liftIO $ handleResponse chain [block]
      {-forM_ (sendMessage localNode) mMessage-}
      case mMessage of
        Nothing -> return ()
        Just msg -> sendMessage localNode msg

handleResponse:: MVar  BlockChain -> BlockChain -> IO (Maybe Msg)
handleResponse localChain chainResponse = do 
  case head chainResponse of
    Nothing -> return Nothing
    Just latestBlockReceived -> do
      mLocalBlock <- getLatestBlock localChain
      case mLocalBlock of
        Nothing -> return Nothing
        Just latestBlockHeld
          | blockIndex latestBlockReceived > blockIndex latestBlockHeld -> do
            prepareResponse latestBlockReceived latestBlockHeld
          | otherwise -> do
            return Nothing 
    where
      prepareResponse latestBlockReceived latestBlockHeld
        | blockHash latestBlockHeld  == previousHash latestBlockReceived = do
          addBlockMVar latestBlockReceived localChain
          return $ Just $ RespLatestBlock latestBlockReceived
        | otherwise  = do 
          setChain localChain chainResponse
          return $ Just $ RespLatestBlock latestBlockReceived

sendMessage :: MonadIO m => LocalNode -> Msg -> m ()
sendMessage localNode message  = liftIO $ runProcess localNode $ do
  P2P.nsendPeers p2pServiceName $ message

-- | Get the latest block from the chain
--
getLatestBlock :: MVar BlockChain -> IO (Maybe Block)
getLatestBlock chain = head <$> readMVar chain

addBlockMVar :: Block -> MVar BlockChain -> IO ()
addBlockMVar b = flip modifyMVar_ (return . addBlock b) 

-- | Replaces local block chain if new chain is longer
setChain :: MVar BlockChain -> BlockChain -> IO ()
setChain chain newChain = modifyMVar_ chain $ \oldChain ->
  case replaceChain oldChain newChain of
    Nothing -> return newChain
    Just err -> putStrLn err >> return oldChain

-- | Returns Nothing if chain should be replaced
replaceChain :: BlockChain -> BlockChain -> Maybe Text 
replaceChain oldChain newChain = case isBrokenChain newChain of
  Nothing 
    | length newChain > length oldChain -> Nothing
    | otherwise -> Just "replaceChain: invalid chain"
  Just err -> Just err 
