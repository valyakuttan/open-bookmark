{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
----------------------------------------------------------------------
-- |
-- Module : Api.Internal
--
----------------------------------------------------------------------


module Api.Internal
    (
      -- * Basic Types to work with clouds
      BookmarkCloud
    , TagCloud

      -- * File system utilities
    , bookmarkCloudPath
    , tagCloudPath
    , bookmarkCloudDirectory
    , tagCloudDirectory

      -- * Environment manipulation
    , getConfig
    , getRootDir
    , getCurrentTime

      -- * Query cloud
    , searchBookmark
    , searchTag

      -- * Bookmark cloud manipulation
    , insertBookmark
    , insertTag

      -- * Read/Write
    , readBookmarkCloud
    , readBookmarkCloudWithDefault
    , readTagCloud
    , readTagCloudWithDefault
    , writeBookmarkCloud
    , writeTagCloud
    ) where


import           Control.Applicative

import           App
import           Cloud.Config
import           Cloud.Core.Bookmarkable
import           Cloud.Core.Engine       (Cloud, JSONData,
                                          bookmarkableToJSONData,
                                          taggableToJSONData)
import qualified Cloud.Core.Engine       as C
import           Cloud.Core.Taggable
import           Cloud.Utils


newtype BookmarkCloud = BC { bc :: (FilePath, Cloud) }
newtype TagCloud = TC { tc :: (FilePath, Cloud) }

-- | Search a bookmark in the cloud. Returns @('Just' v)@
-- if present, or 'Nothing' otherwise.
searchBookmark :: Bookmarkable b => b
               -> App (Maybe JSONData)
searchBookmark b = search'. unwrap <$> readBookmarkCloud b
  where
      search' = C.search $ bookmarkableToJSONData b
      unwrap  = snd . bc

-- | Search a tag in the cloud. Returns @('Just' v)@
-- if present, or 'Nothing' otherwise.
searchTag :: Taggable b => b
          -> App (Maybe JSONData)
searchTag b = search'. unwrap <$> readTagCloud b
  where
      search' = C.search $ taggableToJSONData b
      unwrap  = snd . tc

-- | Insert a bookmark into the cloud. Returns the modifed cloud
-- if cloud is bookmark's destination and it is modified, otherwise
-- return original cloud.
insertBookmark :: Bookmarkable b => b
               -> BookmarkCloud -> App BookmarkCloud
insertBookmark b c = do
    path <- bookmarkCloudPath b
    time <- getCurrentTime
    let bj = bookmarkableToJSONData b
    case isCompatible path (bc c) of
        Just c' -> BC <$> return (path, C.insert time bj c')
        Nothing -> return c

-- | Insert a bookmark into the cloud. Returns the modifed cloud
-- if cloud is bookmark's destination and it is modified, otherwise
-- return original cloud.
insertTag :: Taggable b => b
               -> TagCloud -> App TagCloud
insertTag b c = do
    path <- tagCloudPath b
    time <- getCurrentTime
    let bj = taggableToJSONData b
    case isCompatible path (tc c) of
        Just c' -> TC <$> return (path, C.insert time bj c')
        Nothing -> return c

-- | Read a bookmark cloud from disk. If the operation
-- fails it returns a default cloud.
readBookmarkCloudWithDefault :: Bookmarkable b => b -> App BookmarkCloud
readBookmarkCloudWithDefault b = readBookmarkCloud b <|> BC <$> dc
  where
      dc = (,) <$> path <*> (defaultCloud <$> currentEnvironment)
      path = bookmarkCloudPath b

-- | Read a bookmark cloud from disk.
readBookmarkCloud :: Bookmarkable b => b -> App BookmarkCloud
readBookmarkCloud b = BC <$> pair
  where
      pair = (,) <$> path <*> (path >>= readJSON)
      path = bookmarkCloudPath b

-- | Write a bookmark cloud to disk.
writeBookmarkCloud :: BookmarkCloud -> App ()
writeBookmarkCloud = uncurry writeJSON . bc

-- | Read a tag cloud from disk. If the operation
-- fails it returns a default cloud.
readTagCloudWithDefault :: Taggable b => b -> App TagCloud
readTagCloudWithDefault b = readTagCloud b <|> TC <$> dc
  where
      dc = (,) <$> path <*> (defaultCloud <$> currentEnvironment)
      path = tagCloudPath b

-- | Read a tag cloud from disk.
readTagCloud :: Taggable b => b -> App TagCloud
readTagCloud b = TC <$> pair
  where
      pair = (,) <$> path <*> (path >>= readJSON)
      path = tagCloudPath b

-- | Write a tg cloud to disk.
writeTagCloud :: TagCloud -> App ()
writeTagCloud = uncurry writeJSON . tc

bookmarkCloudPath :: Bookmarkable b => b -> App FilePath
bookmarkCloudPath b =
    bookmarkCloudFilePath <$> getConfig <*> getRootDir <*> pure b

bookmarkCloudDirectory :: App FilePath
bookmarkCloudDirectory =
    bookmarkCloudDirectoryPath <$> getConfig <*> getRootDir

tagCloudPath :: Taggable b => b -> App FilePath
tagCloudPath b = tagCloudFilePath <$> getConfig <*> getRootDir <*> pure b

tagCloudDirectory :: App FilePath
tagCloudDirectory = tagCloudDirectoryPath <$> getConfig <*> getRootDir

isCompatible :: FilePath -> (FilePath, a) -> Maybe a
isCompatible path (path', cloud) | path == path' = Just cloud
                                 | otherwise = Nothing

getConfig :: App Config
getConfig = config <$> currentEnvironment

getRootDir :: App FilePath
getRootDir = rootDir <$> currentEnvironment

getCurrentTime :: App POSIXMicroSeconds
getCurrentTime = currentTime <$> currentEnvironment
