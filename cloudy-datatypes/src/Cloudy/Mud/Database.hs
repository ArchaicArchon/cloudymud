{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Cloudy.Mud.Database where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (catch)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable

import Control.Exception
import Control.Monad (forever,join,when)
--import Data.Hashable
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
--import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Network.CGI.Protocol (maybeRead)
import System.IO hiding (catch)
--import qualified System.IO.Strict as Strict
--import Text.Show.Pretty

import Data.Binary
import Data.Typeable
import GHC.Generics

import Cloudy.Mud.Misc


maybeReadFile :: FilePath -> IO (Maybe BS.ByteString)
maybeReadFile filepath = do 
  catch (BS.readFile filepath >>= return . Just) maybeReadFileHandler 

maybeReadFileHandler :: SomeException -> IO (Maybe BS.ByteString)
maybeReadFileHandler e = do
  when debug . BS.putStrLn 
    $ "In maybeReadFileHandler there was an Exception: " `BS.append` (BS.pack . show $ e)
  return Nothing

maybeWriteFile :: FilePath -> BS.ByteString -> IO Bool
maybeWriteFile filepath contents = do
  catch (BS.writeFile filepath contents >> return True) maybeWriteFileHandler

maybeWriteFileHandler :: SomeException -> IO Bool
maybeWriteFileHandler e = do
  when debug . BS.putStrLn 
    $ "In maybeWriteFileHandler there was an Exception: " `BS.append` (BS.pack . show $ e)
  return False

data Insert a b = Insert a b
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary a, Binary b) => Binary (Insert a b)

data Lookup a = Lookup ProcessId a
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary a) => Binary (Lookup a)

data Delete a = Delete a
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary a) => Binary (Delete a)

data GetAll = GetAll ProcessId 
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary GetAll)

data SaveData a = SaveData a
  deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary a) => Binary (SaveData a)

--databaseActor :: (Serializable k,Serializable v,Ord k) => Map.Map k v -> Process ()
databaseActor database = receiveWait [match insert,match lookup,match delete,match getAll]
  where
  --insert :: (Serializable k,Ord k) => Insert k v -> Process ()
  insert (Insert key value) = do 
    when debug . liftIO . BS.putStrLn 
      $ "Insert " `BS.append` (BS.pack . show $ key) 
      `BS.append` " " `BS.append` (BS.pack . show $ value)
    databaseActor (Map.insert key value database)
  --lookup :: (Serializable k,Ord k) => Lookup k -> Process ()
  lookup (Lookup senderPid key) = do
    when debug . liftIO . BS.putStrLn 
      $ "Lookup " `BS.append` (BS.pack . show $ key)
    let maybeValue = Map.lookup key database
    send senderPid maybeValue
    databaseActor database
  --delete :: (Serializable k,Ord k) => Delete k -> Process ()
  delete (Delete key) = do 
    when debug . liftIO . BS.putStrLn 
      $ "Delete " `BS.append` (BS.pack . show $ key)
    databaseActor (Map.delete key database)
  getAll (GetAll senderPid) = do
    when debug . liftIO . BS.putStrLn
      $ "GetAll" 
    send senderPid database
    databaseActor database 

persistentDatabaseActor maybeReadType filepath = do
  createPersistentDatabaseActor maybeReadType (Map.empty) filepath
  where
  createPersistentDatabaseActor :: 
    (Binary k,Binary v,Typeable k,Typeable v,Show k,Show v,Eq k,Ord k) => 
    (BS.ByteString -> Maybe (Map.Map k v)) -> 
    (Map.Map k v) -> FilePath -> Process () 
  createPersistentDatabaseActor maybeReadType emptyMap filepath = do
    saveActorPid <- spawnLocal $ persistentSaveActor filepath
    maybeFileContents <- liftIO $ maybeReadFile filepath 
    case maybeFileContents of
      Just fileContents -> do
        let maybeDatabase = maybeReadType fileContents
        case maybeDatabase of
          Just database -> persistentDatabaseActor' saveActorPid database
          Nothing -> persistentDatabaseActor' saveActorPid emptyMap
      Nothing -> persistentDatabaseActor' saveActorPid emptyMap
  persistentDatabaseActor' saveActorPid database = do
    receiveWait [match insert,match lookup,match delete]
    where
    --insert :: (Serializable k,Ord k) => Insert k v -> Process ()
    insert (Insert key value) = do 
      when debug . liftIO . BS.putStrLn 
        $ "Insert " `BS.append` (BS.pack . show $ key) 
        `BS.append` " " `BS.append` (BS.pack . show $ value)
      let database' = Map.insert key value database
      send saveActorPid (SaveData . BS.pack . show $ database')
      persistentDatabaseActor' saveActorPid database'
    --lookup :: (Serializable k,Ord k) => Lookup k -> Process ()
    lookup (Lookup senderPid key) = do
      when debug . liftIO . BS.putStrLn 
        $ "Lookup " `BS.append` (BS.pack . show $ key)
      let maybeValue = Map.lookup key database
      send senderPid maybeValue
      persistentDatabaseActor' saveActorPid database
    --delete :: (Serializable k,Ord k) => Delete k -> Process ()
    delete (Delete key) = do 
      when debug . liftIO . BS.putStrLn 
        $ "Delete " `BS.append` (BS.pack . show $ key)
      let database' = Map.delete key database
      send saveActorPid (SaveData . BS.pack . show $ database')
      persistentDatabaseActor' saveActorPid database'

persistentSaveActor filepath = do
  receiveWait [match saveData]
  where
  saveData msg@(SaveData filecontents) = do
    result <- liftIO $ maybeWriteFile filepath filecontents
    case result of
      True -> do
        persistentSaveActor filepath
      False -> do
        liftIO . threadDelay $ defaultTimeout
        saveData msg

