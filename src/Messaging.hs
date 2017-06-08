{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging  where

import Protolude 
import Chain

data Msg 
  = QueryLatestBlock
  | QueryBlockChain
  | RespBlockChain BlockChain
  | RespLatestBlock Block   
  deriving (Generic)

messageHandler chain QueryLatestBlock =  do
  mBlock <- liftIO $  getLatestBlock chain
  case mBlock of
    Just block -> return $  Just  $ RespLatestBlock block 
    Nothing -> return Nothing 
messageHandler chain QueryBlockChain = do 
  mChain <- liftIO $ readMVar chain
  return $ Just $ RespBlockChain mChain
messageHandler chain ( RespLatestBlock block ) = do 
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
          return $ Just $ RespLatestBlock latestBlockReceived
        | otherwise  = do 
          setChain localChain chainResponse
          return $ Just $ RespLatestBlock latestBlockReceived

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
