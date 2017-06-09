{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging  where

import Protolude 
import Common
import Chain

data Msg 
  = MsgQueryLatestBlock
  | MsgQueryBlockChain
  | MsgBlockChain BlockChain
  | MsgLatestBlock Block   
  deriving (Generic)

messageHandler chain MsgQueryLatestBlock =  do
  mBlock <- liftIO $  getLatestBlock chain
  case mBlock of
    Just block -> return $  Just  $ MsgLatestBlock block 
    Nothing -> return Nothing 
messageHandler chain MsgQueryBlockChain = do 
  mChain <- liftIO $ readMVar chain
  return $ Just $ MsgBlockChain mChain
messageHandler chain ( MsgLatestBlock block ) = do 
    mMessage <- liftIO $ handleResponse chain [block]
    return mMessage

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
          return $ Just $ MsgLatestBlock latestBlockReceived
        | otherwise  = do 
          setChain localChain chainResponse
          return $ Just $ MsgLatestBlock latestBlockReceived
