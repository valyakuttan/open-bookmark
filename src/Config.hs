{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config (
      Config
    , bookmarkCloudPrefix
    , bookmarksDirectory
    , dataDirectory
    , defaultConfig
    , tagCloudPrefix
    , tagsDirectory
    , maximumNumberOfClouds
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config {
      bookmarkCloudPrefix :: !Text
    , bookmarksDirectory :: !Text
    , dataDirectory :: !Text
    , tagCloudPrefix :: !Text
    , tagsDirectory :: !Text
    , maximumNumberOfClouds :: Int
    } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

defaultConfig :: Config
defaultConfig = Config
                "bookmark-cloud-"
                "data/bookmarks"
                "data"
                "tag-cloud-"
                "data/tags"
                10
