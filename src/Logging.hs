module Logging (
  initLogger,
  liftDebug
) where

import Control.Monad.Trans
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler               (setFormatter)
import System.Log.Handler.Simple
import System.IO                        (stdout)

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

