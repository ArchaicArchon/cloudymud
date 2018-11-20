{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

import System.FSNotify
import Control.Concurrent (threadDelay)
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
import Network
import qualified Network.Socket as NS
import System.IO

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


generateHostName :: NS.ServiceName -> (HostName, NS.ServiceName)
generateHostName serviceName = (frontHostname,serviceName)

config = WatchConfig {
  confDebounce = Debounce 3,
  confPollInterval = 1,
  confUsePolling = False
  }

coreModified :: Event -> Bool
coreModified (Modified filepath time something) = 
  filepath == "/home/vagrant/.cabal/bin/cloudy-core"
coreModified _ = False 


chanReader :: Control.Concurrent.Chan.Chan Event -> Process ()
chanReader channel = do
  liftIO $ print "chanReader is running!"
  event <- liftIO $ Control.Concurrent.Chan.readChan channel
  liftIO . BS.putStrLn . BS.pack . show $ event
  liftIO . BS.putStrLn . BS.pack . show $ "Core Modified!"
  liftIO . hFlush $ stdout 
  chanReader channel


main :: IO ()
main = do
  BS.putStrLn "Starting Cloudy Watcher (Version 0.0.1)...."
  Right t <- createTransport watcherHostname watcherPort generateHostName defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  BS.putStrLn "We will now start the cloudy watcher...."
  channel <- Control.Concurrent.Chan.newChan
  forkProcess node (chanReader channel) 
  print "spawned chanReader"
  print "starting manager...."
  print "do we get run?" 
  withManager $ \mgr -> do
    watchDirChan mgr "/home/vagrant/.cabal/bin" coreModified channel       -- action
    forever . threadDelay $ (1000*1000)
  print "ran manager does this ever get run?" 
  print "This SHOULD NEVER BE PRINTED/EXECUTED!"
  return () 
    


