{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Common  where

import Protolude
import Chain

-- | Get the latest block from the chain
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
