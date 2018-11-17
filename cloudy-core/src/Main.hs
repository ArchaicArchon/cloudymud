{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Lens


import Network
import qualified Network.Socket as NS

import Data.Binary
import Data.Typeable
import GHC.Generics

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Writer.Lazy

import Control.Applicative
import qualified Control.Exception as Exception
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
--import Database.Bolt
import Data.Default
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid
import System.Microtimer

import Cloudy.Mud.Misc

import Cloudy.Mud.Datatypes

import Cloudy.Mud.Core.Parser


{-
TODO: rewrite everything from a graph database to a map of maps
because the code to get the answers from the query is just too verbose
and the graph database is way too slow at times
2 seconds for initial queries
-}


{- 
Lens helper: 
Sets: 
mw2 = mw & ix (Player "megabug") . at FieldRoomDescription ?~ _MudString # "bjorka"
Gets:  
mw2 ^? ix (Player "megabug") . ix FieldRoomDescription . _MudString
Modifies: 
mw2 = mw & ix (Player "megabug") . ix FieldGroundDescription . _MudString %~ (BS.map toUpper)
-}

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
a +++ b = a `BS.append` b
infixr 5 +++

bsShow :: (Show a) => a -> BS.ByteString
bsShow = BS.pack . show 

debugOut :: BS.ByteString -> Process ()
debugOut byteString = do
  when debug . liftIO . BS.putStrLn $ byteString
  liftIO $ hFlush stdout

t2bs :: T.Text -> BS.ByteString
t2bs t = BS.pack . T.unpack $ t 

bs2t :: BS.ByteString -> T.Text
bs2t bs = T.pack . BS.unpack $ bs

{-
databaseConfig :: BoltCfg
databaseConfig = def {user="megabug",password="mudpass",socketTimeout=0}
-}

generateHostName :: NS.ServiceName -> (HostName, NS.ServiceName)
generateHostName serviceName = (frontHostname,serviceName)


data Event 
	= EventLook Id BS.ByteString
	| EventLeaves Id BS.ByteString
	| EventArrives Id BS.ByteString
	| EventNotify BS.ByteString
 deriving (Show,Eq,Ord,Generic,Typeable)
instance (Binary Event)


data Id 
  = Player BS.ByteString
  | Room BS.ByteString Integer Integer Integer
  deriving (Read,Show,Eq,Ord,Generic,Typeable)
instance (Binary Id)
 -- identifier for actors/processes and objects in the database

id2name :: Id -> Maybe BS.ByteString
id2name (Player string) = Just string
id2name _ = Nothing

data MudField 
  = FieldId
  | FieldActor  
  | FieldContents
  | FieldRoomTitle
  | FieldRoomDescription
  | FieldGroundDescription
  | FieldLocation 
  | FieldExits 
  deriving (Read,Show,Eq,Ord,Generic,Typeable)
instance (Binary MudField)

data MudValue
  = MudId Id
  | MudString BS.ByteString
  | MudListOfId [Id]
  | MudLocation Location
  | MudMapOfExits (Map.Map BS.ByteString MudExit)
  deriving (Read,Show,Eq,Ord,Generic,Typeable)
instance (Binary MudValue)

data MudExit 
  = Blocked
  | Exit Id
  deriving (Read,Show,Eq,Ord,Generic,Typeable)
instance (Binary MudExit)

data Location = Location Id BS.ByteString 
  deriving (Read,Show,Eq,Ord,Generic,Typeable)
instance (Binary Location)


location2id :: MudValue -> Maybe Id
location2id (MudLocation (Location location specifier)) = Just location
location2id _ = Nothing 

$(makePrisms ''MudValue)

type MudWorld = Map.Map Id (Map.Map MudField MudValue)

room0 :: Map.Map MudField MudValue
room0 = Map.fromList [
  (FieldId,MudId (Room "main" 0 0 0)),
  (FieldRoomTitle,MudString "Central Square"),
  (FieldRoomDescription,MudString "This is the center of the city\nIt all begins here..."),
  (FieldContents,MudListOfId [(Player "megabug"),(Player player2name)]),
  (FieldExits,MudMapOfExits (Map.fromList []))
  ]

room1 :: Map.Map MudField MudValue
room1 = Map.fromList [
  (FieldId,MudId (Room "main" 1 0 0)),
  (FieldRoomTitle,MudString "Eastern Boulevard"),
  (FieldRoomDescription,MudString "The road stretches off to the east here...."),
  (FieldContents,MudListOfId []),
  (FieldExits,MudMapOfExits (Map.fromList []))
  ]

playerMegabug :: Map.Map MudField MudValue
playerMegabug = Map.fromList [
  (FieldId,MudId (Player "megabug")),
  (FieldActor,MudString "player"),
  (FieldGroundDescription,MudString "megabug is standing here."),
  (FieldLocation,MudLocation (Location (Room "main" 0 0 0) "contents")),
  (FieldContents,MudListOfId [])
  ]

player2name :: BS.ByteString
player2name = "pinniped"

player2 :: Map.Map MudField MudValue
player2 = Map.fromList [
  (FieldId,MudId (Player player2name)),
  (FieldActor,MudString "player"),
  (FieldGroundDescription,MudString (BS.append player2name " is standing here.")),
  (FieldLocation,MudLocation (Location (Room "main" 0 0 0) "contents")),
  (FieldContents,MudListOfId [])
  ]



initMudWorld :: MudWorld 
initMudWorld = Map.fromList . zip (fmap byId list) $ list
  where
  list = [room0,room1,playerMegabug,player2]
  byId :: Map.Map MudField MudValue -> Id 
  byId item = fromJust $ item ^? ix FieldId . _MudId 

type ActorType = Id  
	-> TVar (Map.Map Id ProcessId) -> TVar MudWorld 
	-> Process ()

scriptToActor :: BS.ByteString -> ActorType
scriptToActor "player" = playerActor
scriptToActor _        = nopActor

{-
world = [
	Script (Player "megabug") PlayerScript,
	Script (Player "nik") PlayerScript,
	Exists (Room 0 0 0),
	RoomTitle (Room 0 0 0) "The World Stage",
	RoomDescription (Room 0 0 0) "You stand on the stage engulfed by all the possibilities.",
	Location (Player "megabug") (Room 0 0 0),
	GroundDescription (Player "megabug") "megabug idly stands here.",
	Location (Player "nik") (Room 0 0 0),
	GroundDescription (Player "nik") "nik solemnly stands here."
	]
-}

main :: IO ()
main = do
  BS.putStrLn "Starting Cloudy Core Server (Version 0.0.1)...."
  Right t <- createTransport coreHostname corePort generateHostName defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  BS.putStrLn "We will now start the core...."
  mainLoopPid <- forkProcess node initialization
  forever $ threadDelay (1000*1000*1000)


nopActor :: ActorType
nopActor identifier tvActorMap tvMudWorld = return ()

initialization :: Process ()
initialization = do
	liftIO . BS.putStrLn $ "Connecting to Graph Database..."
	-- dbPipe <- liftIO . connect $ databaseConfig
	liftIO . BS.putStrLn $ "...Connected to Graph Database"
	liftIO . BS.putStrLn $ "We are now populating actors..."
	-- tvPlayerMap <- liftIO . atomically . newTVar $ (Map.empty :: Map.Map BS.ByteString Id)
	tvActorMap <- liftIO . atomically . newTVar $ (Map.empty :: Map.Map Id ProcessId)
	tvMudWorld <- liftIO . atomically . newTVar $ initMudWorld
	{-
	playerIdRecords <- liftIO . run dbPipe . query 
		$ "match (p:Player) return p.playerName as name, p.id as id"
	liftIO . print $ playerIdRecords
	-}

	{-
	let playerIdList = catMaybes . fmap (\record -> 
		(,) <$> ((fmap t2bs $ record `at` "name" >>= exact)::Maybe BS.ByteString) 
			<*> ((record `at` "id" >>= exact)::Maybe Int)) $ playerIdRecords
	liftIO . mapM_ print $ playerIdList
	liftIO . atomically $ writeTVar tvPlayerMap (Map.fromList playerIdList)
	-}

	{-
	actorIdScriptRecords <- liftIO . run dbPipe . query
		$ "match (a:Actor) return a.id as id , a.actor as script"
	liftIO . print $ actorIdScriptRecords
	-}
	let mudKeys = Map.keys initMudWorld
	let actorIdScriptList = catMaybes
		. fmap (\(maybeIdentifier,maybeScript)-> (,) <$> maybeIdentifier <*> maybeScript) 
		. fmap (\key->(Just key,(initMudWorld ^? ix key . ix FieldActor . _MudString))) 
		$ mudKeys
	liftIO . mapM_ print $ actorIdScriptList

	-- let idPlayerMap = Map.fromList . fmap swap $ playerIdList 
	-- close dbPipe
	
	idActorList <- mapM (\(identifier,script) -> do
		-- actorDbPipe <- connect databaseConfig
		actorPid <- spawnLocal ((scriptToActor script) identifier tvActorMap tvMudWorld)
		return (identifier,actorPid)) actorIdScriptList 
	let idActorMap = Map.fromList idActorList 
	liftIO . atomically . writeTVar tvActorMap $ idActorMap
	
	-- populate the tvar with this
	-- query database for actor's and id's
	-- start actors 
	-- populate the tvar with this
	
	{-
	tvWorld <- liftIO . atomically . newTVar $ world 
	let scriptMapList = do
		Script identifier script <- world
		return (identifier,script)
	actorMapList <- mapM (\(identifier,script)-> do
		actorPid <- spawnLocal ((scriptToActor script) identifier tvActorMap tvWorld) 
		return (identifier,actorPid) ) scriptMapList 
	let actorMap = Map.fromList actorMapList
	liftIO . atomically . writeTVar tvActorMap $ actorMap
	-}
	liftIO . BS.putStrLn $ "Done starting actors!"
	liftIO . BS.putStrLn $ "Starting Demultiplexer..."
	demultiplexPid <- spawnLocal $ demultiplex tvActorMap
	register "demultiplex" demultiplexPid
	liftIO . BS.putStrLn $ "Messaging all Actors that we are up"
	actorMap <- liftIO . atomically . readTVar $ tvActorMap
	mapM_ (\pid-> send pid (EventNotify "The Core is Up!")) actorMap 


-- TODO: removing playerMap needs to be done 
demultiplex :: TVar (Map.Map Id ProcessId) -> Process ()
demultiplex tvActorMap = do
	receiveWait [match nameAndUserInput]
	where
	nameAndUserInput msg@(NameAndUserInput name message) = do
		actorMap <- liftIO . atomically . readTVar $ tvActorMap
		let maybeActorPid = Map.lookup (Player name) actorMap
		case maybeActorPid of
			Nothing -> demultiplex tvActorMap
			Just actorPid -> do
				send actorPid msg
				demultiplex tvActorMap

{-
demultiplex :: Process ()
demultiplex = do
	receiveWait [match nameAndUserInput]
	where
	nameAndUserInput (NameAndUserInput name message) = do
		debugOut $ "demultiplex in cloudy-core received a message: " +++ message +++ " from : " +++ name
		nsendRemote frontNode "chatter" (NameAndUserOutput name $ "echo from the core: " +++ message)
		demultiplex
-}
{-
playerActor :: Identifier -> TVar (Map.Map Identifier ProcessId) -> TVar [Relation] -> Process ()
playerActor identifier tvActorMap tvWorld = do
	receiveWait [match nameAndUserInput]
	where
	nameAndUserInput :: NameAndUserInput -> Process ()
	nameAndUserInput (NameAndUserInput name message) = do
		nsendRemote frontNode "chatter" (NameAndUserOutput name $ "echo from playerActor in the core: " +++ message)
		playerActor identifier tvActorMap tvWorld 
-}

playerActor :: ActorType
playerActor identifier tvActorMap tvMudWorld = do
	receiveWait [match nameAndUserInput,match eventHandler]
	where
	nameAndUserInput :: NameAndUserInput -> Process ()
	nameAndUserInput (NameAndUserInput name message) = do
		let parsedMessage = parseCoreCommand message
		case parsedMessage of
			Left err -> do
				nsendRemote frontNode "chatter" (NameAndUserOutput name "parser error!")
			Right CommandLook -> do
				--nsendRemote frontNode "chatter" (NameAndUserOutput name "You look around.")
				playerLook name identifier tvActorMap tvMudWorld 
			Right (CommandGo direction) -> do
				playerGo name identifier tvActorMap tvMudWorld direction
			Right _ -> do
				nsendRemote frontNode "chatter" (NameAndUserOutput name "I do not understand.")
		playerActor identifier tvActorMap tvMudWorld
	eventHandler :: Event -> Process ()
	eventHandler (EventLook lookerId message) = do
		let maybeName = id2name identifier 
		if lookerId == identifier 
			then return ()
			else mapM_ (\name -> playerOutput name message) maybeName 
		playerActor identifier tvActorMap tvMudWorld
	eventHandler (EventArrives arriverId message) = do
		let maybeName = id2name identifier 
		if arriverId == identifier 
			then return ()
			else mapM_ (\name -> playerOutput name message) maybeName 
		playerActor identifier tvActorMap tvMudWorld
	eventHandler (EventLeaves leaverId message) = do
		let maybeName = id2name identifier 
		if leaverId == identifier 
			then return ()
			else mapM_ (\name -> playerOutput name message) maybeName 
		playerActor identifier tvActorMap tvMudWorld
	eventHandler (EventNotify message) = do
		let maybeName = id2name identifier
		mapM_ (\name -> playerOutput name message) maybeName 
		playerActor identifier tvActorMap tvMudWorld

	eventHandler _ = playerActor identifier tvActorMap tvMudWorld


maybeHead :: [a] -> Maybe a
maybeHead (x:xs) = Just x
maybeHead [] = Nothing 

playerOutput name message = nsendRemote frontNode "chatter" (NameAndUserOutput name message)


maybeItemsInRoom :: Id -> MudWorld -> Maybe [Id] -- takes a player Id 
maybeItemsInRoom identifier mudWorld = 
	let maybeCurrentLocation = join 
		. fmap location2id 
		$ (mudWorld ^? ix identifier . ix FieldLocation) in
	let maybeItems = join 
		. fmap (\roomId -> mudWorld ^? ix roomId . ix FieldContents . _MudListOfId) 
		$ maybeCurrentLocation in 
	maybeItems  

maybeItemsToActors :: (Map.Map Id ProcessId) -> Maybe [Id] -> Maybe [ProcessId]
maybeItemsToActors actorMap maybeItemList = 
	let maybeActorList = fmap (catMaybes . fmap (\item -> Map.lookup item actorMap)) maybeItemList in
	maybeActorList

sendToMaybeActorList :: Maybe [ProcessId] -> Event -> Process ()
sendToMaybeActorList maybeActorList event = 
	mapM_ (mapM_ (\actor -> send actor event)) maybeActorList

sendToActorsInRoom :: Map.Map Id ProcessId -> MudWorld -> Id -> Event -> Process () -- to room that contains identifier
sendToActorsInRoom actorMap mudWorld identifier event = do 
	let maybeItemList = maybeItemsInRoom identifier mudWorld
	let maybeActorList = maybeItemsToActors actorMap maybeItemList
	sendToMaybeActorList maybeActorList event

playerLook :: BS.ByteString -> Id -> TVar (Map.Map Id ProcessId) -> TVar MudWorld -> Process ()
playerLook playerName identifier tvActorMap tvMudWorld = do 
	(mudWorld2,actorMap2) <- liftIO . atomically $ do 
		mudWorld <- readTVar tvMudWorld
		actorMap <- readTVar tvActorMap
		return (mudWorld,actorMap)
	playerOutput playerName $ genericPlayerLook identifier mudWorld2
	sendToActorsInRoom actorMap2 mudWorld2 identifier 
		$ (EventLook identifier (BS.append playerName " looks around."))

genericPlayerLook :: Id -> MudWorld -> BS.ByteString
genericPlayerLook identifier mudWorld = 
	let maybeCurrentLocation = join 
		. fmap location2id 
		$ (mudWorld ^? ix identifier . ix FieldLocation) in
	let roomTitle = maybe "NO TITLE" id . join 
		. fmap (\location -> mudWorld ^? ix location . ix FieldRoomTitle . _MudString)
		$ maybeCurrentLocation in
	let roomDescription = BS.lines . maybe "NO DESCRIPTION" id . join
		. fmap (\location -> mudWorld ^? ix location . ix FieldRoomDescription . _MudString)
		$ maybeCurrentLocation in
	let roomContents = maybe [] id . join
		. fmap (\location -> mudWorld ^? ix location . ix FieldContents . _MudListOfId)
		$ maybeCurrentLocation in
	let roomContentsDescriptions = catMaybes
		. fmap (\item -> mudWorld ^? ix item . ix FieldGroundDescription . _MudString)
		$ roomContents in
	let maybeRoomExits = join . fmap (\room -> getRoomExits mudWorld room) $ maybeCurrentLocation in
	let roomExits = maybe [] id . fmap Map.keys $ maybeRoomExits in 
	let formattedRoomExits = fmap ("  " <>) roomExits in
	let exitOutput = "Exits: " : (if formattedRoomExits == [] then ["  None"] else formattedRoomExits) in
	BS.intercalate newline 
		$ roomTitle:roomDescription ++ roomContentsDescriptions ++ exitOutput
	-- playerOutput playerName "Core processed and caught a look command"
	-- should I use a liftIO here? it seems to work
	{-
	(t1,eitherLookRecords) <- liftIO . time . Exception.try . run dbPipe $ queryP (
		"match (p:Object {id:{identifier}}) " <>
		"match (r:Room)-[:Contains]->(p) " <>
		"match (r)-[:Contains]->(item) " <>
		"match (r)-[e:Exit]->(room2) " <>
		"return r.roomTitle as roomTitle, r.roomDescription as roomDescription, " <> 
		"collect(item.groundDescription) as groundDescriptions, " <>
		"collect(item.id) as idList, " <>
		"collect(distinct {direction : e.direction , title : room2.roomTitle}) as exits"
		) (Map.fromList [("identifier",I identifier)])
	liftIO . outputError "Caught error in playerLook: " $ eitherLookRecords
	let maybeLookRecords = eitherToMaybe eitherLookRecords
	--playerOutput playerName (BS.pack . show $ maybeLookRecords)
	{-
	eitherTimeIdRecords) <- liftIO . time . run dbPipe $ queryP (
		"match (p:Object {id:{identifier}}) " <>
		"match (r:Room)-[:Contains]->(p) " <>
		"match (r)-[:Contains]->(item) " <>
		"return collect(item.id)as idList"
		) (Map.fromList [("identifier",I identifier)])
	playerOutput playerName (BS.pack . show $ idRecords)
	-}
	-- playerOutput playerName "Done processing look command"
	let maybeRecord = join . fmap maybeHead $ maybeLookRecords
	let roomTitle = maybe "NO TITLE" id . fmap t2bs 
		. join . fmap (\record -> record `at` "roomTitle" >>= exact) $ maybeRecord
	let roomDescription = maybe "NO DESCRIPTION" id . fmap t2bs 
		. join . fmap (\record -> record `at` "roomDescription" >>= exact) $ maybeRecord
	let groundDescriptions = maybe [] id . fmap (fmap t2bs) 
		. join . fmap (\record -> record `at` "groundDescriptions" >>= exact) $ maybeRecord
	let idList = maybe [] id 
		. join . fmap (\record -> record `at` "idList" >>= exact) $ maybeRecord
	let exits = maybe [] id
		. join . fmap (\record -> record `at` "exits" >>= exact) $ maybeRecord
	let direction item = maybe "NO DIRECTION" id . fmap t2bs
		. (\record -> record `at` "direction" >>= exact) $ item
	let title item =  maybe "NO TITLE" id . fmap t2bs
		. (\record -> record `at` "title" >>= exact) $ item
	let exitsToDisplay = fmap (\item -> direction item <> " - " <> title item) exits
	actorMap <- liftIO . atomically . readTVar $ tvActorMap
	let maybePidList = fmap (\k -> Map.lookup k actorMap) idList
	mapM_ (\maybePid -> mapM_ (\pid -> 
		send pid (EventLook identifier $ playerName <> " looks around.")) maybePid) maybePidList
	playerOutput playerName . BS.intercalate newline 
		$ [roomTitle,roomDescription] ++ groundDescriptions ++ exitsToDisplay ++ ["You look around."]
	results playerName $ t1
	-}

idVecAdd :: Id -> (Integer,Integer,Integer) -> Maybe Id
idVecAdd (Room zone x1 y1 z1) (x2,y2,z2) = Just (Room zone (x1+x2) (y1+y2) (z1+z2))
idVecAdd _ _ = Nothing 

generateRoomExits :: Id -> Map.Map BS.ByteString MudExit
generateRoomExits room = 
	let north = (Just "north",idVecAdd room (0,1,0)) in
	let east = (Just "east",idVecAdd room (1,0,0)) in
	let south = (Just "south",idVecAdd room (0,(-1),0)) in
	let west = (Just "west",idVecAdd room ((-1),0,0)) in
	let up = (Just "up",idVecAdd room (0,0,1)) in
	let down = (Just "down",idVecAdd room (0,0,(-1))) in
	Map.fromList 
		. fmap (\(k,v) -> (k,Exit v))
		. catMaybes 
		. fmap (\(k,v) -> (,) <$> k <*> v) 
		$ [north,east,south,west,up,down]


maybe2Bool :: Maybe a -> Bool
maybe2Bool (Just x) = True
maybe2Bool Nothing = False

getRoomExits :: MudWorld -> Id -> Maybe (Map.Map BS.ByteString MudExit)
getRoomExits mudWorld roomId = 
	let maybeRoomExits = mudWorld ^? ix roomId . ix FieldExits . _MudMapOfExits in
	let generalExits = generateRoomExits roomId in
	let maybeUnifiedExits = fmap (\exits -> Map.union exits generalExits) maybeRoomExits in
	fmap (\exits -> Map.filter removeNonExits exits) maybeUnifiedExits
	where
	removeNonExits :: MudExit -> Bool
	removeNonExits Blocked = False
	removeNonExits (Exit room) = 
		let maybeRoom = mudWorld ^? ix room in
		maybe2Bool maybeRoom 


results :: BS.ByteString -> Double -> Process ()
results playerName t = playerOutput playerName $ "Results in " <> (BS.pack . formatSeconds $ t)

newline = "\r\n"

outputError :: String -> Either SomeException a -> IO () 
outputError string (Left e) = putStrLn $ string ++ (displayException e) 
outputError string _ = return ()

playerGo :: BS.ByteString -> Id -> TVar (Map.Map Id ProcessId) -> TVar MudWorld -> BS.ByteString -> Process ()
playerGo playerName identifier tvActorMap tvMudWorld direction = do
	-- actorMap <- liftIO . actomically . readTVar  $ tvActorMap
	successfullyMoved <- liftIO . atomically $ do
		mudWorld <- readTVar tvMudWorld
		actorMap <- readTVar tvActorMap
		let returnValueOfGo = genericPlayerGo identifier direction mudWorld
		case returnValueOfGo of 
			Right mudWorld2 -> do
				writeTVar tvMudWorld  mudWorld2
				return $ Right (actorMap,mudWorld,mudWorld2)
			Left errorMessage -> do
				return $ Left errorMessage 
	-- put the output message sends here
	case successfullyMoved of 
		Right (actorMap2,originalMudWorld,newMudWorld) -> do 
			playerOutput playerName ("You go " `BS.append` direction `BS.append` ".")
			playerOutput playerName (genericPlayerLook identifier newMudWorld)
			sendToActorsInRoom actorMap2 originalMudWorld identifier
				$ EventLeaves identifier (BS.append playerName " leaves.")
			sendToActorsInRoom actorMap2 newMudWorld identifier 
				$ EventArrives identifier (BS.append playerName " arrives.")  
		Left errorMessage -> playerOutput playerName "You cannot go that way."
	return ()
	{-
	playerOutput playerName $ "You try to go : " <> direction
	(t1,eitherExitRecords) :: (Double,Either SomeException [Record]) <- liftIO 
		. time . Exception.try . run dbPipe $ queryP (
			"match (p:Object {id:{identifier}}) " <>
			"match (r:Room)-[located:Contains]->(p) " <>
			"match (r)-[e:Exit {direction:{direction}}]->(r2:Room) " <>
			"delete located " <>
			"merge (r2)-[:Contains]->(p)" <>
			--"return e as exit"
			"return true" -- a test
			) (Map.fromList [("identifier",I identifier),("direction",T (bs2t direction))])
	playerOutput playerName . BS.pack . show $ eitherExitRecords
	liftIO . outputError "Caught error in playerGo: " $ eitherExitRecords
	let maybeExitRecords = eitherToMaybe eitherExitRecords
	let maybeRecord = join . fmap (maybeHead) $ maybeExitRecords
	case maybeRecord of
		Nothing -> do
			playerOutput playerName "You cannot go that way."
		Just _ -> do
			playerOutput playerName $ "You attempt to go " <> direction <> "."
	-}
	--return ()

{- Cypher query for full room look:
match (p:Player {playerName:"megabug"})<-[:Contains]-(room) with room as r 
match (r)-[:Contains]->(item) 
match (r)-[e:Exit]->(room2) 
return r.roomTitle as title, r.roomDescription as description, 
	collect(item.groundDescription) as groundDescriptions, 
	collect({direction : e.direction ,title : room2.roomTitle}) as exits
-}

type ErrorGenericPlayerGo = BS.ByteString -- change this type maybe to be more specific

genericPlayerGo :: Id -> BS.ByteString -> MudWorld -> Either ErrorGenericPlayerGo MudWorld 
genericPlayerGo identifier direction mudWorld = 
	let maybeCurrentLocation = join 
		. fmap location2id 
		$ (mudWorld ^? ix identifier . ix FieldLocation) in
	let maybeRoomExits = join . fmap (getRoomExits mudWorld) $ maybeCurrentLocation  in 
	let maybeNewRoomId = join . fmap (Map.lookup direction) $ maybeRoomExits in
	case maybeNewRoomId of 
		Just (Exit newRoomId) ->
			Right $ move mudWorld identifier (Location newRoomId "contents")
		_ ->
			Left "You cannot go that way!"

removeFromContents :: MudWorld -> Id -> Id -> MudWorld 
removeFromContents mudWorld locationId item = 
	let mudWorld2 = mudWorld & ix locationId . ix FieldContents . _MudListOfId 
		%~ (filter (/= item )) in
	mudWorld2 

addToContents :: MudWorld -> Id -> Id -> MudWorld 
addToContents mudWorld newLocationId item =
	let mudWorld2 = mudWorld & ix newLocationId . ix FieldContents . _MudListOfId
		%~ (\list -> list ++ [item]) in
	mudWorld2

move :: MudWorld -> Id -> Location -> MudWorld 
move mudWorld item newLocation = 
	let maybeLocation = mudWorld ^? ix item . ix FieldLocation . _MudLocation in
	case (maybeLocation,newLocation) of
		((Just (Location locationId "contents")),(Location newLocationId "contents")) ->
			let mudWorld2 = removeFromContents mudWorld locationId item in
			let mudWorld3 = addToContents mudWorld2 newLocationId item in 
			let mudWorld4 = mudWorld3 & ix item . ix FieldLocation . _MudLocation .~ newLocation in
			mudWorld4
		_ -> mudWorld
