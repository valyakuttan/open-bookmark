{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

----------------------------------------------------------------------
-- |
-- Module : Cloud.Config
--
----------------------------------------------------------------------


module Cloud.Config (
      Config
    , cloudFilePath
    , bookmarkDirectoryPath
    , tagDirectoryPath
    , configDirectoryPath
    , configFilePath
    , defaultConfig
    , maxNumberOfClouds
    ) where


import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import System.FilePath ((</>))

import Cloud.Types


newtype Config = Config (Map String String)
               deriving (Show, Generic)

-- | Returns the file path  where this item wil be stored.
cloudFilePath :: Storable b => Config -> FilePath -> b -> FilePath
cloudFilePath config@(Config cfg) root b = case storeType b of
    "book" -> dir </> name
       where
           dir = bookmarkDirectoryPath root
           name = p ++ show i ++ suffix
           p   = M.findWithDefault "bookmark-cloud-" bookPrefix cfg
           i   = cloudIndex config b
    "tag" -> dir </> name
       where
           dir  = tagDirectoryPath root
           name = p ++ show i ++ suffix
           p    = M.findWithDefault "tag-cloud-" tagPrefix cfg
           i    = cloudIndex config b
    _      -> error "Unknown Storable"

  where
    bookPrefix = "bookmark_cloud_prefix"
    tagPrefix  = "tag_cloud_prefix"
    suffix     = ".json"

cloudIndex :: Storable b => Config -> b -> Int
cloudIndex cfg b = hash b `mod` maxNumberOfClouds cfg

maxNumberOfClouds :: Config -> Int
maxNumberOfClouds (Config cfg)
    = read $ M.findWithDefault "500" "max_number_of_clouds" cfg

-- | Retrurns the directory where bookmarks are stored.
bookmarkDirectoryPath :: FilePath -> FilePath
bookmarkDirectoryPath root = cloudHome root </> "bookmarks"

-- | Retrurns the directory where tags are stored.
tagDirectoryPath :: FilePath -> FilePath
tagDirectoryPath root = cloudHome root </> "tags"

configFilePath :: FilePath -> FilePath
configFilePath root = configDirectoryPath root </> configFileName

configDirectoryPath :: FilePath -> FilePath
configDirectoryPath root = root </> "config"

configFileName :: String
configFileName = "open-bookmark.json"

-- | Retrurns the directory where data will be stored.
cloudHome :: FilePath -> FilePath
cloudHome root =  root </> "data"

defaultConfig :: Config
defaultConfig = Config $ M.fromList [
     ("max_number_of_clouds", "500")
   , ("bookmark_cloud_prefix", "bookmark-cloud-")
   , ("tag_cloud_prefix", "tag-cloud-")
   ]

instance ToJSON Config
instance FromJSON Config
