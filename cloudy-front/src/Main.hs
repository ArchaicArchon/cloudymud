{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

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
import qualified Data.Map as Map
--import qualified Data.HashMap.Strict as HashMap
import Network.CGI.Protocol (maybeRead)
import System.IO hiding (catch)

import Text.Parsec hiding (spaces)

import Network
import qualified Network.Socket as NS

import Data.Binary
import Data.Typeable
import GHC.Generics

import Database.Bolt
import Data.Default
import qualified Data.Text as T
import qualified Data.Map as Map

import Cloudy.Mud.Database
import Cloudy.Mud.Misc

import Cloudy.Chat.Parser

import Cloudy.Mud.Datatypes
{-
add support for regular telnet clients other then netcat
who use \r\n for end of line <- Should be DONE

add monitoring to processes instead of bidirectional links like Future Erlang
add commands: chat, tell, who, quit (forward other messages to another node with another handler)

register the databaseActor as names to connections or somethin so the other node can discover it

-}
{- TODO: BUGFIX
If someone connects first with a name "X",
then someone else connects with name "X",
then the first connection quits or disconnects
the second connection loses "contact" with the server
but stays connected.
To fix: make when a name connects it first kills and removes any other connection
with that name. That should fix it

-}

tcpPort :: PortNumber
tcpPort = 5000


(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
a +++ b = a `BS.append` b
infixr 5 +++

bsShow :: (Show a) => a -> BS.ByteString
bsShow = BS.pack . show 

debugOut :: BS.ByteString -> Process ()
debugOut byteString = do
  when debug . liftIO . BS.putStrLn $ byteString
  liftIO $ hFlush stdout

generateHostName :: NS.ServiceName -> (HostName, NS.ServiceName)
generateHostName serviceName = (frontHostname,serviceName)

--hostName = "127.0.0.1"

--serviceName = "10501"

main :: IO () 
main = do
  BS.putStrLn "Starting Cloudy Chat Server (Version 0.0.1)...."
  Right t <- createTransport frontHostname frontPort generateHostName defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  BS.putStrLn "We will now start chat actors...."
  mainLoopPid <- forkProcess node initialization
  forever $ threadDelay (1000*1000*1000)

initialization :: Process ()
initialization = do
    liftIO . BS.putStrLn $ "Starting chatter databse"
    chatDatabasePid <- spawnLocal $ databaseActor (Map.empty :: Map.Map BS.ByteString ProcessId)
    chatterPid <- spawnLocal $ chatter chatDatabasePid
    register "chatter" chatterPid
    listeningSocket <- liftIO . listenOn $ PortNumber tcpPort
    liftIO . BS.putStrLn $ "Listening on port " `BS.append` (BS.pack . show $ tcpPort)
    listeningSocketHandler chatDatabasePid listeningSocket

listeningSocketHandler :: ProcessId -> Socket -> Process ()
listeningSocketHandler chatDatabasePid listeningSocket = do
  liftIO $ BS.putStrLn "Accepting connections now..."
  (socketHandle, _, _) <- liftIO $ accept listeningSocket
  debugOut "Inbound connection..."
  liftIO $ hSetBuffering socketHandle NoBuffering
  liftIO $ BS.hPutStrLn socketHandle "Connected....\r"
  _ <- spawnLocal $ initializeConnection chatDatabasePid socketHandle
  listeningSocketHandler chatDatabasePid listeningSocket

chatter :: ProcessId -> Process () 
chatter chatDatabasePid = do
  receiveWait [match nameAndUserOutput]
  where
  nameAndUserOutput (NameAndUserOutput name message) = do
    self <- getSelfPid
    send chatDatabasePid (GetAll self)
    maybeDatabase <- expectTimeout defaultTimeout :: (Process (Maybe (Map.Map BS.ByteString ProcessId)))
    case maybeDatabase of
      Nothing -> chatter chatDatabasePid
      Just database -> do
        let maybeConnectionHandlerPid = Map.lookup name database
        case maybeConnectionHandlerPid of
          Nothing -> chatter chatDatabasePid
          Just connectionHandlerPid -> do
            send connectionHandlerPid (UserOutput message)
            chatter chatDatabasePid 


nopLoop :: Process ()
nopLoop = 
  liftIO . forever . threadDelay $ (10*1000*1000)

initializeConnection :: ProcessId -> Handle -> Process ()
initializeConnection chatDatabasePid socketHandle = do
  debugOut "Initalizing Connection...."
  dummyPid <- spawnLocal $ nopLoop
  connectionManagerPid <- spawnLocal $ connectionManager chatDatabasePid dummyPid 
  debugOut $ "spawned connectionManager as " +++ bsShow connectionManagerPid
  connectionHandlerPid <- spawnLocal $ connectionHandler dummyPid dummyPid connectionManagerPid 
  debugOut $ "spawned connectionHandler as " +++ bsShow connectionHandlerPid
  inputHandlerPid  <- spawnLocal $ inputHandler connectionHandlerPid socketHandle
  debugOut $ "spawned inputHandler as " +++ bsShow inputHandlerPid
  outputHandlerPid <- spawnLocal $ outputHandler socketHandle
  debugOut $ "spawned outputHandler as " +++ bsShow outputHandlerPid
  send connectionHandlerPid (SetInputHandler inputHandlerPid)
  send connectionHandlerPid (SetOutputHandler outputHandlerPid)
  send connectionHandlerPid StartMonitoring
  send connectionManagerPid (SetConnectionHandler connectionHandlerPid)
  send connectionManagerPid Initialize
  -- kill dummyPid "The use is through for my dummy process" -- is this breaking it?
  when debug . liftIO . BS.putStrLn $ "Done Initializing Connection." 
  -- TODO: perhaps add monitoring to processes here
  --when debug $ send outputHandlerPid (UserOutput "Testing output of outputHandler\n")
  --when debug $ send connectionHandlerPid (UserOutput "Testing output of connectionHandler\n")
  -- add stuff for multiway process shutdown using monitoring HERE

chatBroadcast :: ProcessId -> BS.ByteString -> Process ()
chatBroadcast chatDatabasePid message = do
  selfPid <- getSelfPid
  send chatDatabasePid (GetAll selfPid)
  maybeDatabase <- expectTimeout defaultTimeout :: (Process (Maybe (Map.Map BS.ByteString ProcessId)))
  case maybeDatabase of
    Just database -> do
      let connections = Map.elems database
      mapM_ (\connection -> send connection (UserOutput message)) connections
    Nothing -> do
      debugOut "chatBroadcast: Timeout talking to chatDatabase"
      return ()


-- TODO: change over to multiline bytestring message when it is added to
--   outputHandler and connectionHandler
chatWho :: ProcessId -> ProcessId -> Process ()
chatWho chatDatabasePid connectionHandlerPid = do
	selfPid <- getSelfPid
	send chatDatabasePid (GetAll selfPid)
	maybeDatabase <- expectTimeout defaultTimeout :: (Process (Maybe (Map.Map BS.ByteString ProcessId)))
	case maybeDatabase of
		Just database -> do
			let names = Map.keys database
			send connectionHandlerPid (UserOutput "These people are logged in:")
			mapM_ (\name -> send connectionHandlerPid (UserOutput $ "  " +++ name)) names
		Nothing ->
			debugOut "chatWho: Timeout talking to chatDatabase"


-- TODO: FIX: not receiving the ProcessId associated with recepient nom
--   Wonder why.... hmmmm
chatTell :: ProcessId -> ProcessId -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Process ()
chatTell chatDatabasePid connectionHandlerPid name nom msg = do
	selfPid <- getSelfPid
	send chatDatabasePid (Lookup selfPid nom)
	maybeMaybeRecepientPid <- expectTimeout defaultTimeout :: (Process (Maybe (Maybe ProcessId)))
	case maybeMaybeRecepientPid of
		Just (Just recepientPid) -> do
			send recepientPid (UserOutput $ name +++ " tells you, \"" +++ msg +++ "\"")
			send connectionHandlerPid (UserOutput $ "You tell " +++ nom +++ ", \"" +++ msg +++ "\"")
		whatever -> do
			send connectionHandlerPid (UserOutput $ nom +++ " is unavailable for now.")

-- This seems to be a key function for current program functionality
-- Considering renaming it, but to what?
-- TODO: need to possibly parse username input for leading, trailing, 
--  and whitepsace in the middle
-- TODO: need to add process monitoring of connection handler
--   to connectionManager and connectionManager' 
connectionManager :: ProcessId -> ProcessId -> Process ()
connectionManager chatDatabasePid connectionHandlerPid = 
  receiveWait [match setConnectionHandler,match initialize]
  where
  setConnectionHandler msg@(SetConnectionHandler chp) = do
    debugOut $ "connectionManager " +++ bsShow msg
    connectionManager chatDatabasePid chp 
  initialize Initialize = do
    debugOut "connectionManager Initialize"
    send connectionHandlerPid (UserOutput "Welcome to Cloudy Chat!")
    send connectionHandlerPid (UserOutput "Please enter your name: ")
    maybeUserInput <- expectTimeout (30*1000*1000):: (Process (Maybe UserInput))
    -- the long timeout gives them 30 seconds to chose a name
    case maybeUserInput of 
      Just (UserInput newName) -> do
        connectionManagerPid <- getSelfPid
        spawnLocal (watcherOfHandlerAndManager newName chatDatabasePid connectionHandlerPid connectionManagerPid)
        send connectionHandlerPid (UserOutput 
          $ "Hello, " +++ newName +++ ", what is your password: ")
        --add stuff for monitoring and removing our manager here
        maybeUserPassword <- expectTimeout (30*1000*1000):: (Process (Maybe UserInput))
        case maybeUserPassword of
          Just (UserInput userPassword) -> do
            --pipe <- connect $ def { user = "megabug", password = "mudpass" }
            {-
            databasePasswords <- run pipe . query 
              $ "match (p {playerName :\"" `T.append` (T.pack . BS.unpack $ newName) `T.append` "\"}) return p.password"
            close pipe 
            let maybePassword = fmap (BS.pack . T.unpack).join.fmap toText . join.fmap (\x->x `at` "p.password") $ maybeHead databasePasswords
            -}
            case (userPassword == userPassword) of --FIX this when I figure out how to handle and store passwords
              True -> do 
                send chatDatabasePid (Insert newName connectionHandlerPid)
                chatBroadcast chatDatabasePid ("::: WELCOME " +++ newName +++ " :::")
                connectionManager' chatDatabasePid connectionHandlerPid newName
              False -> do
                send connectionHandlerPid (UserOutput "")
                send connectionHandlerPid (UserOutput "INVALID PASSWORD")
                send connectionHandlerPid (UserOutput "Disconnecting...")
                liftIO . threadDelay $ (3*1000*1000)
                send connectionHandlerPid Die
          Nothing -> do
            send connectionHandlerPid (UserOutput "")
            send connectionHandlerPid (UserOutput "Come back when you can enter your password faster")
            liftIO . threadDelay $ (3*1000*1000)
            send connectionHandlerPid Die  
      Nothing -> do
        send connectionHandlerPid (UserOutput "")
        send connectionHandlerPid (UserOutput "Come back when you have chosen a name")
        liftIO . threadDelay $ (3*1000*1000)
        send connectionHandlerPid Die
        --perhaps I should close the connection if Nothing matches

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

toText :: Value -> Maybe T.Text
toText v = exact v 

connectionManager' :: ProcessId -> ProcessId -> BS.ByteString -> Process ()
connectionManager' chatDatabasePid connectionHandlerPid name = 
  receiveWait [match userInput]
  where 
  -- TODO: modify this handler to do these functions: chat, tell, who, quit, 
  --   and eventually forward to another manager on another node.
  -- TODO: lets do command parsing with an actual parser, like Parsec
  userInput (UserInput byteString) = do
    let parsed = parseChatCommand byteString
    case parsed of
      (Right (ChatChat msg)) -> 
        chatBroadcast chatDatabasePid (name +++ ": " +++ msg)
      (Right (ChatTell nom msg)) ->
        chatTell chatDatabasePid connectionHandlerPid name nom msg
      (Right (ChatWho)) -> 
        chatWho chatDatabasePid connectionHandlerPid 
      (Right (ChatQuit)) -> do {
        send connectionHandlerPid (UserOutput "Come back soon!") ;
        die ( "connectionManager' dying from ChatQuit"::String ) }
      whatever -> 
        nsendRemote coreNode "demultiplex" (NameAndUserInput name byteString) 
    connectionManager' chatDatabasePid connectionHandlerPid name 

-- FUTURE TODO: perhaps add setRemoteConnectionManager,
--   which messages ConnectionManager(')
connectionHandler :: ProcessId -> ProcessId -> ProcessId -> Process ()
connectionHandler inputHandlerPid outputHandlerPid connectionManagerPid = 
  receiveWait 
    [match setInputHandler,match setOutputHandler,match userInput,
      match userOutput,match userOutputPrompt,
      match startMonitoring,match monitorNotification,
      match onDie]
  where
  setInputHandler msg@(SetInputHandler ihp) = do
    debugOut $ "connectionHandler " +++ bsShow msg
    connectionHandler ihp outputHandlerPid connectionManagerPid
  
  setOutputHandler msg@(SetOutputHandler ohp) = do
    debugOut $ "connectionHandler " +++ bsShow msg
    connectionHandler inputHandlerPid ohp connectionManagerPid 
  
  userInput (UserInput byteString) = do
    debugOut $ "connectionHandler (UserInput " +++ byteString +++ ")"
    send connectionManagerPid (UserInput byteString)
    connectionHandler inputHandlerPid outputHandlerPid connectionManagerPid
  
  userOutput (UserOutput byteString) = do
    debugOut $ "connectionHandler (UserOutput " +++ byteString +++ ")"
    send outputHandlerPid (UserOutput byteString)
    connectionHandler inputHandlerPid outputHandlerPid connectionManagerPid
  
  userOutputPrompt (UserOutputPrompt byteString) = do
    debugOut $ "connectionHandler (UserOutput " +++ byteString +++ ")"
    send outputHandlerPid (UserOutputPrompt byteString)
    connectionHandler inputHandlerPid outputHandlerPid connectionManagerPid

  --startMonitoring :: StartMonitoring -> Process ()
  startMonitoring StartMonitoring = do
    monitor inputHandlerPid
    monitor outputHandlerPid
    connectionHandler inputHandlerPid outputHandlerPid connectionManagerPid

  monitorNotification :: ProcessMonitorNotification -> Process ()
  monitorNotification (ProcessMonitorNotification monitorRef pid diedReason) = do
    kill inputHandlerPid "ConnectionHandler Died"
    kill outputHandlerPid "ConnectionHandler Died"
    die ("ConnectionHandler dying from a ProcessMonitorNotification" :: String)

  onDie :: Die -> Process ()
  onDie Die = do
    kill inputHandlerPid "ConnectionHandler Died"
    send outputHandlerPid Die
    die ("ConnectionHandler dying from Die message" :: String)

watcherOfHandlerAndManager name chatDatabasePid connectionHandlerPid connectionManagerPid = do
  monitor connectionHandlerPid
  monitor connectionManagerPid 
  receiveWait [match monitorNotification]
  where
  monitorNotification :: ProcessMonitorNotification -> Process ()
  monitorNotification (ProcessMonitorNotification monitorRef pid diedReason) = do
    send chatDatabasePid (Delete name)
    chatBroadcast chatDatabasePid ("::: GoodBye " `BS.append` name `BS.append` " :::")
    send connectionHandlerPid Die
    kill connectionManagerPid "watcherOfHandlerAndManager has died from a ProcessMonitorNotification"
    die ("watcherOfHandlerAndManager has died from a ProcessMonitorNotification"::String)

inputHandler :: ProcessId -> Handle -> Process ()
inputHandler connectionHandlerPid socketHandle = do
  line <- liftIO $ BS.hGetLine socketHandle
  let line' = endTrim line
  debugOut $ "inputHandler (UserInput " +++ line' +++ ")"
  send connectionHandlerPid (UserInput line')
  inputHandler connectionHandlerPid socketHandle 
  where
  -- perhaps this function should be broken out
  endTrim byteString =
    let string = BS.unpack byteString in
    let string' = reverse . dropWhile (=='\r') . reverse $ string in
    BS.pack string'

--TODO: Add UserOutputMultiline and UserOutputPromptMultiline
outputHandler :: Handle -> Process () 
outputHandler socketHandle = 
  receiveWait [match userOutput,match userOutputPrompt,
               match onDie]
  where
  userOutput (UserOutput byteString) = do
    debugOut $ "outputHandler (UserOutput " +++ byteString +++ ")"
    liftIO . BS.hPutStr socketHandle $ byteString
    liftIO $ BS.hPutStr socketHandle "\r\n"
    liftIO $ hFlush socketHandle
    outputHandler socketHandle
  userOutputPrompt (UserOutputPrompt byteString) = do
    debugOut $ "outputHandler (UserOutputPrompt " +++ byteString +++ ")"
    liftIO . BS.hPutStr socketHandle $ byteString
    --liftIO $ BS.hPutStr handle "\r\n"
    liftIO $ hFlush socketHandle
    outputHandler socketHandle
  onDie Die = do
    liftIO . hClose $ socketHandle
    die ("outputHandler dying from Die message" :: String)
