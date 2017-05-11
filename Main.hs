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
import Text.Read (readMaybe)
-- for path unpack : Text->String
import qualified Data.Text as T

-- for url timestamp salt
import System.Random (newStdGen, randomR)

-- for showHex
import Numeric (showHex)

main = do
    forkIO httpMain
    serve (Host "127.0.0.1") "8888" processTCP

processTCP (socket,remoteAddr) = do
                             putStrLn $ "TCP from " ++ show remoteAddr
                             -- sqlite3 text limit is 2147483647, or 2.1GB
                             -- accept up to 500KB:
                             paste <- recv socket 500000
                             case paste of
                                Nothing -> return ()
                                Just p -> insertPaste p socket


insertPaste :: C.ByteString -> Socket -> IO ()
insertPaste string socket = do 
               conn <- connectSqlite3 "baluster.db"
               posixTimestamp <- getPOSIXTime
               let timestamp = round $ posixTimestamp::Int
               generator <- newStdGen -- create a random generator
               let ifd = (fst $ randomR (0, 16777215) generator) ::Integer
               let id = (showHex $ ifd) "" 
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


slugToNumber :: [T.Text] -> Maybe String
slugToNumber s = case B16.decode . C.pack $ slug of
                  (_, "") -> Just slug
                  _ -> Nothing
                 where slug = T.unpack (s !! 0)

idToText :: String -> IO (Maybe String)
idToText i = do
          conn <- connectSqlite3 "baluster.db"
          r <- Database.HDBC.quickQuery' conn "SELECT content FROM baluster WHERE id = ?;" [Database.HDBC.toSql i]
          case length r of
            0 -> return Nothing
            _ -> return $ Just $ convRow $ r !! 0
          where 
            convRow :: [Database.HDBC.SqlValue] -> String
            convRow [c] = case Database.HDBC.fromSql c of 
                            Just x -> x
                            Nothing -> "NULL"
            convRow x = fail $ "unexpected result: " ++ show x 

app req respond = do
                let slug = pathInfo req
                case length slug of
                  1 -> case slugToNumber slug of
                          Just i -> do
                                  text <- idToText i
                                  respond $ index i text
                          Nothing -> respond nay
                  _ -> respond nay

nay = responseBuilder status400 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "Accepted url: http://localhost:9999/<number>\n"
    , "Pipe your paste to localhost:8888\n" ]

index i Nothing =  responseBuilder status404 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ "paste with id ", BU.fromString $ show i, " not found"]
index i (Just text) = responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ BU.fromString text  ]
