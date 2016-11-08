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
import Control.Monad (liftM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.Default
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
    ("Accept","application/vnd.twitchtv.v3+json"),
    ("Client-ID", clientID)
    ]}

addOauthData :: B.ByteString -> Request -> Request
addOauthData oauth req = 
        let headers = requestHeaders req
        in case oauth of
            "" -> req
            _ -> req { requestHeaders = ("Authorization", oauth):headers }

makeGet :: Request -> Request
makeGet req = requestBase req { method = "GET" }

makePut :: Request -> Request
makePut req = requestBase req { method = "PUT" }

makeRequest :: Url -> (Request -> Request) -> IO [B.ByteString]
makeRequest url requestTransformer = runResourceT $ do
        manager <- liftIO $ newManager tlsManagerSettings
        req <- parseUrl url
        oauth <- liftIO getAuthData
        let req' = addOauthData oauth . requestTransformer $ req
        body <- worker manager req'
        body $$+- CL.consume

worker :: (MonadResource m, MonadIO m) => Manager -> Request -> m (ResumableSource m B.ByteString)
worker manager req = do
        res <- http req manager
        return $ responseBody res

decodeChunks :: (Default a, FromJSON a) => [B.ByteString] -> Maybe a
decodeChunks = decode . L.fromChunks

list :: String ->  IO ()
list "games" = liftM tgTop (listContext "games" Nothing :: IO TopGames) >>= display
list "following" = liftM streams (listContext "following" Nothing :: IO StreamList) >>= display
list notFound = do
    putStrLn $ "unable to list " ++ notFound

listContext :: (Default a, FromJSON a) => String -> Maybe String -> IO a
listContext res query = do
    let q = fromMaybe def query
    l <- makeRequest (getUrl res ++ q) makeGet
    let result = fromMaybe def (decodeChunks l)
    return result

search :: String -> IO ()
search arg = liftM streams (searchStreams (Just arg)) >>= display

watch :: String -> IO ()
watch arg = do
    createProcess (proc "livestreamer" ["twitch.tv/" ++ arg, "best"])
    return ()

getUrl :: String -> Url
getUrl s = fromMaybe def $ M.lookup s urls

searchStreams :: Maybe String -> IO StreamList
searchStreams query = listContext "search" query

followChannel :: String -> IO ()
followChannel channelName = do
        makeRequest (getUrl "follow" ++ channelName) makePut
        putStrLn $ "Followed " ++ channelName

opts :: Parser (IO ())
opts = subparser
        (
            command "list" (info (list <$> argument str (metavar "RESOURCE")) idm) <> 
            command "search" (info (search <$> argument str (metavar "QUERY")) idm) <>
            command "watch" (info (watch <$> argument str (metavar "CHANNEL")) idm) <>
            command "follow" (info (followChannel <$> argument str (metavar "CHANNEL")) idm)
        )
