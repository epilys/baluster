{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import Data.Monoid
import qualified Database.HDBC
import qualified Database.HDBC.Sqlite3
import Data.Time.Clock.POSIX


import Network.Simple.TCP
import Control.Concurrent (forkIO)
import Text.Read (readMaybe)
-- for path unpack : Text->String
import qualified Data.Text as T

main = do
    forkIO httpMain
    serve (Host "127.0.0.1") "8888" processTCP

processTCP (socket,remoteAddr) = do
                             putStrLn $ "TCP from " ++ show remoteAddr
                             paste <- recv socket 2147483647
                             case paste of
                                Just p -> insertPaste p socket
                                Nothing -> return ()

insertPaste :: Data.ByteString.Char8.ByteString -> Socket -> IO ()
insertPaste string socket = do 
               --Data.ByteString.putStrLn string
               conn <- Database.HDBC.Sqlite3.connectSqlite3 "baluster.db"
               posixTimestamp <- getPOSIXTime
               let timestamp = round $ posixTimestamp::Integer
               Database.HDBC.run conn "INSERT INTO baluster (content,timestamp) VALUES (?,?)" [Database.HDBC.toSql $ BU.toString string, Database.HDBC.toSql timestamp]
               Database.HDBC.commit conn
               r <- Database.HDBC.quickQuery' conn "SELECT id FROM baluster WHERE timestamp = ?;" [Database.HDBC.toSql timestamp]
               let stringRows = map convRow r
               let idToReturn =  stringRows !! 0
               let url = "http://localhost:9999/" ++ idToReturn ++ "\n"
               putStrLn $ "Added paste at " ++ url
               send socket $ Data.ByteString.Char8.pack url
               Database.HDBC.disconnect conn
               where 
                convRow :: [Database.HDBC.SqlValue] -> String
                convRow [id] = case Database.HDBC.fromSql id of 
                                Just x -> show (x::Integer)
                                Nothing -> "NULL"
                convRow x = fail $ "unexpected result: " ++ show x 

httpMain :: IO ()                             
httpMain = do
    let port = 9999
    putStrLn $ "Listening on port " ++ show port
    run port app


slugToNumber :: [T.Text] -> Maybe Integer
slugToNumber s = (readMaybe $ T.unpack $ s !! 0) :: Maybe Integer

idToText :: Integer -> IO (Maybe String)
idToText i = do
          conn <- Database.HDBC.Sqlite3.connectSqlite3 "baluster.db"
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
                -- putStrLn $ show $ slug !! 0
                case length slug of
                  1 -> case slugToNumber slug of
                          Just i -> do
                                  text <- idToText i
                                  respond $ index i text
                          Nothing -> respond nay
                  _ -> respond nay

nay = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ mconcat $ map copyByteString
    [ "Accepted url: http://localhost:9999/<number>\n"
    , "Pipe your paste to localhost:8888\n" ]

index i Nothing =  responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ "paste with id ", BU.fromString $ show i, " not found"]
index i (Just text) = responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString
    [ BU.fromString text  ]
