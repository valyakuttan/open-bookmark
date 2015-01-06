{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Config
--
----------------------------------------------------------------------


module Cloud.Config (
      Config (..)
    , defaultConfig
    , cloudFilePath
    , cloudDirectoryPath
    ) where


import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import           System.FilePath         ((</>))
import Data.Map.Strict

import           Cloud.Core.Types


data Config = Config {
      cloudFilePrefix      :: Map Text Text
    , cloudFileLocation    :: Map Text Text
    , cloudHome            :: !Text
    , maximumNumberOfClouds :: Int
    } deriving (Show, Generic)

cloudFilePath :: Bookmarkable b => Config
                         -> FilePath
                         -> b
                         -> FilePath
cloudFilePath cfg root b = dir </> fileName
  where
      dir      = cloudDirectoryPath t cfg root
      fileName = prefix ++ show index ++ suffix
      prefix = T.unpack (cloudFilePrefix cfg ! k)
      index  = bookmarkHash b `mod` maximumNumberOfClouds cfg
      suffix = ".json"
      t      = bookmarkType b
      k      = T.pack $ show t

cloudDirectoryPath :: BookmarkType -> Config -> FilePath -> FilePath
cloudDirectoryPath t cfg root = dir
  where
      dir  = ddir </> T.unpack (cloudFileLocation cfg ! k)
      ddir = root </> T.unpack (cloudHome cfg)
      k    = T.pack $ show t

instance FromJSON Config
instance ToJSON Config

defaultConfig :: Config
defaultConfig = Config
    (fromList [(book, "bookmark-cloud-"), (tag, "tag-cloud-")])
    (fromList [(book, "bookmarks"), (tag, "tags")])
    "data"
    10
  where
      f = T.pack . show
      book = f Book
      tag = f Tag
