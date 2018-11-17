{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

import System.FSNotify
import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (forever)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (catch)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Chan
import Data.Time.Clock
import Data.Fixed

import Cloudy.Mud.Misc

import Cloudy.Mud.Datatypes

{-
WatchConfig	 

    confDebounce :: Debounce

    Debounce configuration
    confPollInterval :: Int

    Polling interval if polling is used (microseconds)
    confUsePolling :: Bool
-}

config = WatchConfig {
  confDebounce = Debounce 3,
  confPollInterval = 1,
  confUsePolling = False
  }

coreModified :: Event -> Bool
coreModified (Modified filepath time something) = 
  filepath == "/home/vagrant/.cabal/bin/cloudy-core"
coreModified _ = False 

chanReader :: Chan Event -> IO ()
chanReader channel = do
  event <- readChan channel
  print event
  print "Core Modified!"
  chanReader channel

main = do
  channel <- Control.Concurrent.Chan.newChan
  forkIO (chanReader channel) 
  withManagerConf config $ \mgr -> do
    -- start a watching job (in the background)
    watchDirChan
      mgr          -- manager
      "/home/vagrant/.cabal/bin"          -- directory to watch
      coreModified -- predicate
      channel       -- action
    
    --sleep forever (until interrupted)
    forever $ threadDelay 1000000


