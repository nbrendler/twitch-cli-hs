{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Commands  where

import System.Directory
import System.IO.Error
import System.Process
import Control.Exception
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.Maybe
import Data.List

--conduit package
import Data.Conduit
import qualified Data.Conduit.List as CL

--http-conduit package
import Network.HTTP.Conduit

import Data.Aeson (decode, FromJSON)

import Options.Applicative

import Types
import Printer

getAuthData :: IO B.ByteString
getAuthData = do
    home <- getHomeDirectory
    B.readFile (home ++ "/.twitch-cli-hs-oauth-data") `catch` handleDoesNotExist where 
        handleDoesNotExist e 
           | isDoesNotExistError e = return "" 
           | otherwise = throwIO e
            

clientID :: B.ByteString
clientID = "ff8j1xobdatp0psh0qkkmbziyv1y9td"


requestBase :: Request -> Request
requestBase req = req {requestHeaders = [
    ("Accept","application/vnd.twitchtv.v2+json"),
    ("Client-ID", clientID)
    ]}

addOauthData :: B.ByteString -> Request -> Request
addOauthData oauth req = 
        let headers = requestHeaders req
            formatAuthData = case oauth of
                "" -> ("","")
                _ -> ("Authorization", oauth)
            in req { requestHeaders = headers ++ [formatAuthData] }

makeGet :: Request -> Request
makeGet req = requestBase req { method = "GET" }

makePut :: Request -> Request
makePut req = requestBase req { method = "PUT" }

makeRequest :: Url -> (Request -> Request) -> IO [B.ByteString]
makeRequest url requestTransformer = runResourceT $ do
        manager <- liftIO $ newManager tlsManagerSettings
        req <- parseUrl url
        oauth <- liftIO $ getAuthData
        let req' = addOauthData oauth . requestTransformer $ req
        body <- worker manager req'
        body $$+- CL.consume


worker :: (MonadResource m, MonadIO m) => Manager -> Request -> m (ResumableSource m B.ByteString)
worker manager req = do
        res <- http req manager
        return $ responseBody res


decodeChunks :: (FromJSON a) => [B.ByteString] -> Maybe a
decodeChunks = decode . L.fromChunks

list :: String -> IO ()
list arg 
    | arg == "games" = listGames >>= display
    | otherwise = listFollowing >>= display

search :: String -> IO ()
search arg = searchStreams arg >>= display

watch :: String -> IO ()
watch arg = do
    createProcess (proc "livestreamer" ["twitch.tv/" ++ arg, "best"])
    return ()

getUrl :: String -> Url
getUrl s = fromJust $ M.lookup s urls

listGames :: IO [[String]]
listGames = do
    l <- makeRequest (getUrl "topGames") makeGet
    let topGames = fromJust $ (decodeChunks l :: Maybe TopGames)
        gameInfos = tgTop topGames
        displayInfo gi = [(gName . giGame) gi, show (giViewers gi)]
    return $ map displayInfo gameInfos

listFollowing :: IO [[String]]
listFollowing = do
    l <- makeRequest (getUrl "following") makeGet
    let streamList = fromJust $ (decodeChunks l :: Maybe StreamList)
    return $ map displayInfo $ streams streamList where
        displayInfo s = [
            (chName . stChannel) s,
            (chGame . stChannel) s,
            (chStatus . stChannel) s,
            show (stViewers s)
            ]

searchStreams :: String -> IO [[String]]
searchStreams query = do
    l <- makeRequest (getUrl "search" ++ query) makeGet
    let resultList = fromJust $ (decodeChunks l :: Maybe StreamList)
    return $ map displayInfo $ streams resultList where
        displayInfo s = [
            (chName . stChannel) s,
            (chGame . stChannel) s,
            (chStatus . stChannel) s,
            show (stViewers s)
            ]

followChannel :: String -> IO ()
followChannel channelName = do
        makeRequest (getUrl "follow" ++ channelName) makePut
        putStrLn $ "Followed " ++ channelName

opts :: Parser (IO ())
opts = subparser
        (
            command "list" (info (list <$> argument str (metavar "STUFF")) idm) <> 
            command "search" (info (search <$> argument str (metavar "SEARCH")) idm) <>
            command "watch" (info (watch <$> argument str (metavar "CHANNEL")) idm) <>
            command "follow" (info (followChannel <$> argument str (metavar "CHANNEL")) idm)
        )
