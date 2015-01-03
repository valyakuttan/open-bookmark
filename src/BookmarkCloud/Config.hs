{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BookmarkCloud.Config (
      Config
    , bookmarkCloudPrefix
    , bookmarksDirectory
    , dataDirectory
    , defaultConfig
    , tagCloudPrefix
    , tagsDirectory
    , maximumNumberOfClouds
    , bookmarkCloudFilePath
    , tagCloudFilePath
    , bookmarkCloudDirectoryPath
    , tagCloudDirectoryPath
    ) where


import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           System.FilePath                 ((</>))

import           BookmarkCloud.Core.Bookmarkable
import           BookmarkCloud.Core.Taggable


data Config = Config {
      bookmarkCloudPrefix   :: !Text
    , bookmarksDirectory    :: !Text
    , dataDirectory         :: !Text
    , tagCloudPrefix        :: !Text
    , tagsDirectory         :: !Text
    , maximumNumberOfClouds :: Int
    } deriving (Show, Generic)

bookmarkCloudFilePath :: Bookmarkable b => Config
                         -> FilePath
                         -> b
                         -> FilePath
bookmarkCloudFilePath cfg root b = dir </> prefix
  where
      dir    = bookmarkCloudDirectoryPath cfg root
      prefix = bookmarkCloudFile cfg b

tagCloudFilePath :: Taggable t => Config -> FilePath -> t -> FilePath
tagCloudFilePath cfg root t = dir </> prefix
  where
      dir    = tagCloudDirectoryPath cfg root
      prefix = tagCloudFile cfg t

bookmarkCloudDirectoryPath :: Config -> FilePath -> FilePath
bookmarkCloudDirectoryPath cfg root = dir
  where
      dir  = ddir </> T.unpack (bookmarksDirectory cfg)
      ddir = root </> T.unpack (dataDirectory cfg)

tagCloudDirectoryPath :: Config -> FilePath -> FilePath
tagCloudDirectoryPath cfg root = dir
  where
      dir  = ddir </> T.unpack (tagsDirectory cfg)
      ddir = root </> T.unpack (dataDirectory cfg)

bookmarkCloudFile :: Bookmarkable b => Config -> b -> FilePath
bookmarkCloudFile cfg b = prefix ++ suffix
  where
      prefix = T.unpack $ bookmarkCloudPrefix cfg
      suffix = show index ++ ".json"
      index  = bookmarkHash b `mod` maximumNumberOfClouds cfg

tagCloudFile :: Taggable t => Config -> t -> FilePath
tagCloudFile cfg t = prefix ++ suffix
  where
      prefix = T.unpack $ tagCloudPrefix cfg
      suffix = show index ++ ".json"
      index  = tagHash t `mod` maximumNumberOfClouds cfg

instance FromJSON Config
instance ToJSON Config

defaultConfig :: Config
defaultConfig = Config
                "bookmark-cloud-"
                "bookmarks"
                "data"
                "tag-cloud-"
                "tags"
                10
