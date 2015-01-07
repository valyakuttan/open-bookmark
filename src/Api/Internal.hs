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

      -- * File system utilities
    , cloudFilePath
    , cloudDirectory

      -- * Environment manipulation
    , getConfig
    , getRootDir
    , getCurrentTime

      -- * Query cloud
    , search

      -- * Bookmark cloud manipulation
    , insertWith

      -- * Read/Write
    , readCloud
    , readCloudWithDefault
    , writeCloud
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Text           (pack)

import           App
import qualified Cloud.Config        as Cfg
import           Cloud.Core.Engine   (Cloud)
import qualified Cloud.Core.Engine   as Engine
import           Cloud.Types


newtype BookmarkCloud = BC { getCloud :: (FilePath, Cloud) }
                      deriving (Eq)


-- | Search a bookmarkable in the cloud. Returns @('Just' v)@
-- if present, or 'Nothing' otherwise.
search :: Bookmarkable b => b
       -> App (Maybe Bookmark)
search b = do
    c <- readCloud b
    result <- performOp search' b c
    case result of
        Just r -> return r
        Nothing -> return Nothing
  where
      search' = Engine.search $ Engine.bookmarkableToBookmark b

-- | Insert with a function, combining new value and old value.
-- @insertWith f b bookmarkcloud@ will insert b into bookmarkcloud
-- if b does not exist in the bookmarkcloud. Otherwise, the
-- function will insert the value @f new_value old_value@ into
-- the bookmarkcloud.
insertWith :: Bookmarkable b => (Bookmark -> Bookmark -> Bookmark)
           -> b
           -> BookmarkCloud
           -> App BookmarkCloud
insertWith f b c = do
    path <- cloudFilePath b
    time <- getCurrentTime
    let bj = Engine.bookmarkableToBookmark b

    result <- performOp (Engine.insertWith time f bj) b c
    case result of
        Just c' -> BC <$> return (path, c')
        Nothing -> return c

-- | Read a cloud from disk. If the operation
-- fails it returns a default cloud.
readCloudWithDefault :: Bookmarkable b => b -> App BookmarkCloud
readCloudWithDefault b = readCloud b <|> BC <$> pair
  where
      pair = (,) <$> path <*> cloud
      path = cloudFilePath b
      cloud = Engine.emptyCloud <$> pure (bookmarkType b) <*> t <*> t
      t = getCurrentTime

-- | Read a cloud from disk.
readCloud :: Bookmarkable b => b -> App BookmarkCloud
readCloud b = BC <$> pair
  where
      pair = (,) <$> path <*> (path >>= readJSON)
      path = cloudFilePath b

-- | Write a cloud back to disk.
writeCloud :: BookmarkCloud -> App ()
writeCloud = uncurry writeJSON . getCloud

-- | Retruns the path where this bookmarkable will be stored
cloudFilePath :: Bookmarkable b => b -> App FilePath
cloudFilePath b =
    Cfg.cloudFilePath <$> getConfig <*> getRootDir <*> pure b

-- | Returns the home of this bookmark type. This is where
-- the cloud files get stored. The bookmark type can either
-- be 'Book' or 'Tag'.
cloudDirectory :: BookmarkType -> App FilePath
cloudDirectory t = Cfg.cloudDirectoryPath t <$> getConfig <*> getRootDir

performOp :: Bookmarkable b => (Cloud -> a)
          -> b
          -> BookmarkCloud
          -> App (Maybe a)
performOp f b c = do
    let (path,c') = getCloud c
        btype = pack (show (bookmarkType b))

    path' <- cloudFilePath b

    let go ok | ok = return $ Just $ f c'
              | otherwise = return Nothing

    go (path == path' && btype == c' ^. Engine.cloudType)

-- | Returns the current 'Config' object
getConfig :: App Config
getConfig = config <$> currentEnvironment

-- | Returns the root directory of the cloud home.
getRootDir :: App FilePath
getRootDir = rootDir <$> currentEnvironment

-- | Returns current time.
getCurrentTime :: App POSIXMicroSeconds
getCurrentTime = currentTime <$> currentEnvironment
