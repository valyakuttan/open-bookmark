----------------------------------------------------------------------
-- |
-- Module : Cloud.Config
--
----------------------------------------------------------------------


module Cloud.Config (
      Config (..)
    , defaultConfig
    , cloudFilePath
    , bookmarkDirectoryPath
    , tagDirectoryPath
    ) where


import System.FilePath ((</>))
import Data.Map.Strict

import Cloud.Types


data Config = Config {
      cloudFilePrefix      :: !(Map String String)
    , cloudFileLocation    :: !(Map String FilePath)
    , cloudHome            :: !FilePath
    , maximumNumberOfClouds :: Int
    } deriving (Show)

-- | Returns the file path  where this bookmarkable wil be stored.
cloudFilePath :: Storable b => Config -> FilePath -> b -> FilePath
cloudFilePath cfg root b = dir </> name
  where
      dir    = cloudDirectoryPath t cfg root
      name   = prefix ++ show index ++ suffix
      prefix = cloudFilePrefix cfg ! t
      index  = hash b `mod` maximumNumberOfClouds cfg
      suffix = ".json"
      t      = storeType b

-- | Retrurns the directory where bookmarks are stored.
bookmarkDirectoryPath :: Config -> FilePath -> FilePath
bookmarkDirectoryPath = cloudDirectoryPath "book"

-- | Retrurns the directory where tags are stored.
tagDirectoryPath :: Config -> FilePath -> FilePath
tagDirectoryPath = cloudDirectoryPath "tag"

cloudDirectoryPath :: String -> Config -> FilePath -> FilePath
cloudDirectoryPath ctype cfg root = dir
  where
      dir  = ddir </> cloudFileLocation cfg ! ctype
      ddir = root </> cloudHome cfg

defaultConfig :: Config
defaultConfig = Config
    (fromList [("book", "bookmark-cloud-"), ("tag", "tag-cloud-")])
    (fromList [("book", "bookmarks"), ("tag", "tags")])
    "data"
    10
