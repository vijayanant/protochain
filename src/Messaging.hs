{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging (
  Msg(..),
  messageType,
  messageHandler,
) where

import Protolude 
import Common
import Chain

data Msg 
  = MsgQueryLatestBlock
  | MsgQueryBlockChain
  | MsgBlockChain BlockChain
  | MsgLatestBlock Block   
  deriving (Show, Generic)

type MessageType = [Char]

messageType :: Msg -> MessageType
messageType MsgQueryBlockChain    = "MsgQueryBlockChain"
messageType MsgQueryLatestBlock   = "MsgQueryLatestBlock"
messageType ( MsgBlockChain _ )   = "MsgBlockChain"
messageType ( MsgLatestBlock _ )  = "MsgLatestBlock"

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
messageHandler chain (MsgBlockChain chainReceived) = do
  mMessage <- liftIO $ handleResponse chain chainReceived
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
          return Nothing -- $ Just $ MsgLatestBlock latestBlockReceived
        | length chainResponse == 1 = return Nothing
        | otherwise  = do 
          setChain localChain chainResponse
          return Nothing -- $ Just $ MsgLatestBlock latestBlockReceived
