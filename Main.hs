{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai (responseBuilder, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404, status400)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as C

import Data.ByteString.Base16 as B16
import Data.Monoid (mconcat)
import qualified Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Data.Time.Clock.POSIX (getPOSIXTime)


import Network.Simple.TCP (serve, HostPreference(Host), Socket, send, recv)
import Control.Concurrent (forkIO)

-- for path unpack : Text->String
import qualified Data.Text as T

-- for url
import System.Random (newStdGen, randomR)

import Numeric (showHex)

{- 
 - baluster - TCP pastebin server. 
 - * serves pastes over http
 - * accepts pastes over tcp (eg. with netcat)
 - 
 - example usage: echo -n test | nc localhost 8888
 -}
main = do
    forkIO httpMain -- process http requests (serving pastes) in a different thread
    serve (Host "127.0.0.1") "8888" processTCP -- accept TCP connections for incoming pastes

processTCP (socket,remoteAddr) = do
                             putStrLn $ "TCP from " ++ show remoteAddr
                             -- sqlite3 text limit is 2147483647, or 2.1GB
                             -- accept up to 500KB:
                             paste <- recv socket 500000
                             case paste of
                                Nothing -> return ()
                                Just p -> insertPaste p socket


{-
 - Insert ByteString to sqlite3 database, and return paste url to user.
 - * url is random hex
 - * save timestamp to clean up old pastes with a cron job
 -}
insertPaste :: C.ByteString -> Socket -> IO ()
insertPaste string socket = do 
               conn <- connectSqlite3 "baluster.db"
               posixTimestamp <- getPOSIXTime -- get timestamp for db entry
               let timestamp = round $ posixTimestamp::Int -- convert to integer
               generator <- newStdGen -- create a random generator
               let ifd = (fst $ randomR (0, 16777215) generator) ::Integer -- get a random hex for the paste's id and url
               let id = (showHex $ ifd) "" -- showHex returns a ShowS, which is (String ++), so unfold the value by applying "" to it
               Database.HDBC.run conn "INSERT INTO baluster (id, content,timestamp) VALUES (?,?,?)" [Database.HDBC.toSql id, Database.HDBC.toSql $ BU.toString string, Database.HDBC.toSql timestamp]
               Database.HDBC.commit conn
               Database.HDBC.disconnect conn
               let url = "http://localhost:9999/" ++ id ++ "\n"
               putStr $ "Added paste at " ++ url
               send socket $ C.pack url

httpMain :: IO ()                             
httpMain = do
    let port = 9999
    putStrLn $ "Listening on port " ++ show port
    run port app

{-
 - See if we can convert the requested url as a hex number
 - If not, then the requested url is not valid
 -}
slugToNumber :: [T.Text] -> Maybe String
slugToNumber s = case isHex of
                  True -> Just slug
                  False -> Nothing
                 where slug = T.unpack (s !! 0)
                       isHex  = C.foldl (\x c-> x && (isHexChar c)) True $ C.pack slug
                       isHexChar c = ((c>='0') && (c<='9')) || ((c>='a') && (c<='f')) || ((c>='A') && (c<='F'))

{-
 - Query the database for given hex and return the content
 -}
idToText :: String -> IO (Maybe String)
idToText i = do
          conn <- connectSqlite3 "baluster.db"
          r <- Database.HDBC.quickQuery' conn "SELECT content FROM baluster WHERE id = ?;" [Database.HDBC.toSql i]
          case length r of
            0 -> return Nothing
            _ -> return $ convRow $ r !! 0
          where 
            convRow :: [Database.HDBC.SqlValue] -> Maybe String
            convRow [c] = Database.HDBC.fromSql c
            convRow x = fail $ "unexpected result: " ++ show x 

{- 
 - Get the request, extract the paste id (in hex form) from tthe path,
 - and return an appropriate response
 -}
app req respond = do
                let slug = pathInfo req
                case length slug of
                  1 -> case slugToNumber slug of
                          Just i -> do
                                  text <- idToText i -- get the text from the database
                                  respond $ index i text -- send it to user
                          Nothing -> respond nay -- not a valid hex number
                  _ -> respond nay -- wrong url format, or perhaps the mainpage?

{-
 - Return 400 to user with information
 -}
nay = responseBuilder status400 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "Accepted url: http://localhost:9999/<number>\n"
    , "echo -n your paste | nc localhost 8888\n"]

{-
 - Return the paste or 404 if it's not found in the database
 -}
index i Nothing =  responseBuilder status404 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ "paste with id ", BU.fromString $ show i, " not found"]
index i (Just text) = responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ BU.fromString text  ]
