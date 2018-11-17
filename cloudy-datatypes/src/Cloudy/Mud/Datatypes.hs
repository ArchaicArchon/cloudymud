{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Cloudy.Mud.Datatypes where

import Data.Binary
import Data.Typeable
import GHC.Generics

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (catch)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable

import qualified Data.ByteString.Char8 as BS

import Network.Transport



data UserInput = UserInput BS.ByteString
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary UserInput)

fromUserInput :: UserInput -> BS.ByteString
fromUserInput (UserInput byteString) = byteString

data UserOutput= UserOutput BS.ByteString
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary UserOutput)

data UserOutputPrompt = UserOutputPrompt BS.ByteString
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary UserOutputPrompt)

data NameAndUserInput = NameAndUserInput BS.ByteString BS.ByteString
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary NameAndUserInput)

data NameAndUserOutput = NameAndUserOutput BS.ByteString BS.ByteString
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary NameAndUserOutput)

data SetInputHandler = SetInputHandler ProcessId
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary SetInputHandler)

data SetOutputHandler = SetOutputHandler ProcessId
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary SetOutputHandler)

data SetConnectionHandler = SetConnectionHandler ProcessId
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary SetConnectionHandler)

data Initialize = Initialize 
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary Initialize)

data StartMonitoring = StartMonitoring
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary StartMonitoring)

data Die = Die
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary Die)

data CoreModified = CoreModified
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary CoreModified) 

makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr,BS.pack ":0"]

frontHostname = "127.0.0.1"

frontPort = "10500"

frontNode = makeNodeId $ frontHostname ++ ":" ++ frontPort

coreHostname = "127.0.0.1"

corePort = "10600"

coreNode = makeNodeId $ coreHostname ++ ":" ++ corePort

watcherHostname = "127.0.0.1"

watcherPort = "107000"

watcherNode = makeNodeId $ watcherHostname ++ ":" ++ watcherPort
