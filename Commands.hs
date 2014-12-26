{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Commands  where

import System.Directory
import System.IO.Error
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

getAuthData :: IO B.ByteString
getAuthData = do
    B.readFile ".oauth-data" `catch` handleDoesNotExist where 
        handleDoesNotExist e 
           | isDoesNotExistError e = return "" 
           | otherwise = throwIO e
            

clientID :: B.ByteString
clientID = "ff8j1xobdatp0psh0qkkmbziyv1y9td"


getData :: Url -> IO [B.ByteString]
getData s = runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        req <- parseUrl s
        oauth <- liftIO $ getAuthData
        let req' = req {requestHeaders = [
             ("Accept","application/vnd.twitchtv.v2+json"),
             ("Client-ID", clientID),
             formatAuthData
             ]} where
                 formatAuthData = case oauth of
                    "" -> ("","")
                    _ -> ("Authorization", oauth)
                    
        body <- worker manager req'
        body $$+- CL.consume


worker :: (MonadResource m, MonadIO m) => Manager -> Request -> m (ResumableSource m B.ByteString)
worker manager req = do
        res <- http req manager
        return $ responseBody res


decodeChunks :: (FromJSON a) => [B.ByteString] -> Maybe a
decodeChunks = decode . L.fromChunks

display :: [[String]] -> IO ()
display xs = do
    let spacedLines = map (U.fromString . intercalate " ") xs
    mapM_ B.putStrLn spacedLines

list :: String -> IO ()
list arg 
    | arg == "games" = listGames >>= display
    | otherwise = listFollowing >>= display

search :: String -> IO ()
search arg = searchStreams arg >>= display

getUrl :: String -> Url
getUrl s = fromJust $ M.lookup s urls


listGames :: IO [[String]]
listGames = do
    l <- getData $ getUrl "topGames"
    let topGames = fromJust $ (decodeChunks l :: Maybe TopGames)
        gameInfos = tgTop topGames
        displayInfo gi = [(gName . giGame) gi, show (giViewers gi)]
    return $ map displayInfo gameInfos

listFollowing :: IO [[String]]
listFollowing = do
    l <- getData $ getUrl "following"
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
    l <- getData $ getUrl "search" ++ query
    let resultList = fromJust $ (decodeChunks l :: Maybe StreamList)
    return $ map displayInfo $ streams resultList where
        displayInfo s = [
            (chName . stChannel) s,
            (chGame . stChannel) s,
            (chStatus . stChannel) s,
            show (stViewers s)
            ]

opts :: Parser (IO ())
opts = subparser
        (
            command "list" (info (list <$> argument str idm) idm) <> 
            command "search" (info (search <$> argument str idm) idm)
        )
