{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.WebSockets
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

import Network
import Network.Socket 
import System.IO

import Data.Text as T 
import Data.ByteString.Lazy.Internal 
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import Data.Semigroup
import Data.Monoid (mconcat)

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Exception



main :: IO ()
main = do 
  defaultApp <- scottyApp $ do
    get "/" $ do
      file "./index.html"
    get "/clouds.jpg" $ do
    	file "./clouds.jpg"
    get "/files/happy.svg" $ do
    	file "./files/happy.svg"
  let port = 3000
  putStrLn $ "running webserver on port " ++ show port 
  run port (app defaultApp)


app :: Application -> Application
app backupApp = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        ws <- acceptRequest pending_conn
        sendTextData ws ("Hello, client!" :: T.Text)
        socket <- connectTo "127.0.0.1" (PortNumber 5000)
        hSetNewlineMode socket universalNewlineMode
        hSetBuffering socket LineBuffering
        (tidPing,threadDeadPing) <- myForkPingThread ws 5
        (tidReadWebsocket,threadDeadReadWebsocket) <- myForkIO $ do 
            forever (readWebsocket ws socket)
        (tidReadSocket,threadDeadReadSocket) <- myForkIO $ do
            forever (readSocket ws socket)
        {-
        readMVar threadDead1
        readMVar threadDead2
        -}
        {- 
        race_ (forever $ readWebsocket ws socket) (forever $ readSocket ws socket)
        -}

        race_ (readMVar threadDeadPing >> putStrLn "threadDeadPing" >> return ()) 
            (race_ (readMVar threadDeadReadWebsocket >> putStrLn "threadDeadReadWebsocket" >> return ()) 
                (readMVar threadDeadReadSocket >> putStrLn "threadDeadReadSocket" >> return ()))
        
        {-
        readMVar threadDeadReadWebsocket >> putStrLn "threadDeadReadWebsocket" >> return ()
        -}
        putStrLn "Killing client threads..."
        killThread tidPing
        killThread tidReadWebsocket
        killThread tidReadSocket
        putStrLn "Done killing client threads."
        putStrLn "trying to close mud socket...."
        hClose socket
        putStrLn "mudsocket has been CLOSED!!!"
        sendClose ws ("Bye!" :: BSL.ByteString)
        return () 
    readWebsocket websocketConnection socketConnection = do
        message <- receiveDataMessage websocketConnection
        case message of 
            Text byteString _ -> do
                BS.hPutStrLn socketConnection . BS.pack . BSL.unpack $ byteString
                hFlush socketConnection
                {-
                sendTextData connection . (<>) "echo from server: " 
                    . T.pack . BSL.unpack  $ byteString
                -}
            _ -> return ()
    readSocket websocketConnection socketConnection = do
        line <- BS.hGetLine socketConnection
        sendTextData websocketConnection . T.pack . BS.unpack $ line 

{-
fork action = do
    forkIO action
    return ()
-}

myForkIO :: IO () -> IO (ThreadId,MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    tid <- forkFinally io (\_ -> putMVar mvar ())
    return (tid,mvar)


myForkPingThread :: Connection -> Int -> IO (ThreadId,MVar ())
myForkPingThread conn n
    | n <= 0    = do
        dudMvar <- newEmptyMVar
        putMVar dudMvar ()
        tid <- forkIO . return $ () 
        return (tid,dudMvar)
    | otherwise = do
        (tid,mvar) <- myForkIO (go 1) {-(ignore `handle` go 1)-}
        return (tid,mvar)
  where
    go :: Int -> IO ()
    go i = do
        threadDelay (n * 1000 * 1000)
        sendPing conn (T.pack $ show i)
        go (i + 1)

    ignore e = case fromException e of
        Just async -> throwIO (async :: AsyncException)
        Nothing    -> return ()