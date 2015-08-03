{-# LANGUAGE OverloadedStrings #-}

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
    ) where


import Data.Configurator
import Data.Configurator.Types (Config)
import System.FilePath ((</>))

import Cloud.Types


-- | Returns the file path  where this item wil be stored.
cloudFilePath :: Storable b => Config -> FilePath -> b -> IO FilePath
cloudFilePath config root b = case storeType b of
    "book" -> do
        let dir = bookmarkDirectoryPath root
        p   <- lookupDefault "bookmark-cloud-" config bookPrefix
        i   <- cloudIndex config b
        let name = p ++ show i ++ suffix
        return (dir </> name)

    "tag" -> do
        let dir = tagDirectoryPath root
        p <- lookupDefault "tag-cloud-" config tagPrefix
        i  <- cloudIndex config b
        let name = p ++ show i ++ suffix
        return (dir </> name)

    _      -> error "Unknown Storable"

  where
    bookPrefix = "bookmark_cloud_prefix"
    tagPrefix  = "tag_cloud_prefix"
    suffix     = ".json"

cloudIndex :: Storable b => Config -> b -> IO Int
cloudIndex config b =
    (hash b `mod`) <$> lookupDefault 500 config "max_number_of_clouds"

-- | Retrurns the directory where bookmarks are stored.
bookmarkDirectoryPath :: FilePath -> FilePath
bookmarkDirectoryPath root = cloudHome root </> "bookmarks"

-- | Retrurns the directory where tags are stored.
tagDirectoryPath :: FilePath -> FilePath
tagDirectoryPath root = cloudHome root </> "tags"

configDirectoryPath :: FilePath -> FilePath
configDirectoryPath root = root </> "config"

-- | Retrurns the directory where data will be stored.
cloudHome :: FilePath -> FilePath
cloudHome root =  root </> "data"
