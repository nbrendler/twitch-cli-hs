{-# LANGUAGE OverloadedStrings #-}
module Types (
    TopGames (..),
    GameInfo (..),
    Game (..),
    StreamList (..),
    Stream (..),
    Channel (..),
    Url,
    urls
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.Aeson as J
import Data.Aeson (decode, parseJSON, FromJSON, (.:), Value)

type Url = String
type LinkMap = Map.Map String Url

baseUrl :: String
baseUrl = "https://api.twitch.tv/kraken"

urls :: LinkMap
urls = Map.fromList [
    ("topGames", baseUrl ++ "/games/top"),
    ("following", baseUrl ++ "/streams/followed"),
    ("search", baseUrl ++ "/search/streams?q=")
    ]

data TopGames = TopGames {
    tgTotal :: Int,
    tgTop :: [GameInfo],
    tgLinks :: LinkMap
} deriving Show

instance FromJSON TopGames where
    parseJSON (J.Object v) = TopGames <$>
        v .: "_total" <*>
        v .: "top" <*>
        v .: "_links"
    parseJSON _ = mzero

data GameInfo = GameInfo {
    giChannels :: Int,
    giGame :: Game,
    giViewers :: Int
} deriving Show

instance FromJSON GameInfo where
    parseJSON (J.Object v) = GameInfo <$>
        v .: "channels" <*>
        v .: "game" <*>
        v .: "viewers"
    parseJSON _ = mzero

data Game = Game {
    gId :: Int,
    gName :: String
} deriving Show

instance FromJSON Game where
    parseJSON (J.Object v) = Game <$>
        v .: "_id" <*>
        v .: "name"
    parseJSON _ = mzero

data StreamList = StreamList {
    streams :: [Stream]
} deriving Show

instance FromJSON StreamList where
    parseJSON (J.Object v) = StreamList <$> v .: "streams"
    parseJSON _ = mzero

data Stream = Stream {
    stViewers :: Int,
    stChannel :: Channel
} deriving Show

instance FromJSON Stream where
    parseJSON (J.Object v) = Stream <$>
        v .: "viewers" <*>
        v .: "channel"
    parseJSON _ = mzero

data Channel = Channel {
    chName :: String,
    chStatus :: String,
    chGame :: String
} deriving Show
    
instance FromJSON Channel where
    parseJSON (J.Object v) = Channel <$>
        v .: "display_name" <*>
        v .: "status" <*>
        v .: "game"
    parseJSON _ = mzero
