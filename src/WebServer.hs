{-# LANGUAGE OverloadedStrings #-}
module WebServer where

import Protolude
import Chain
import P2P
import Messaging
import Common
import Web.Scotty as S
import Control.Distributed.Process.Node

type HostName = [Char]
type PortNumber = Int


httpServer :: LocalNode ->  HostName ->  PortNumber -> MVar BlockChain ->  IO ()
httpServer localNode host httpPort chain = scotty httpPort $ do 
  
  S.get "/blocks" $ do 
    bs <- liftIO $ readMVar chain
    json bs

  S.get "/mineblock" $ do 
    block <- liftIO $ getLatestBlock chain 
    mNewBlock <- liftIO $  mineBlockAndUpdateChain chain block 
    case mNewBlock of
      Nothing -> return ()
      Just b -> do 
        liftIO $ sendMessage localNode $  MsgLatestBlock b
        json b


mineBlockAndUpdateChain :: MVar BlockChain -> Maybe Block -> IO (Maybe Block)
mineBlockAndUpdateChain _ Nothing  = return Nothing
mineBlockAndUpdateChain chain (Just block) = 
  modifyMVar chain $ \chain'-> do
    ts  <- currentTime
    let newBlock = createNewBlock block ts "" -- empty DATA !!!!!!
    let newChain = addBlock newBlock chain'
    return (newChain, Just newBlock)


