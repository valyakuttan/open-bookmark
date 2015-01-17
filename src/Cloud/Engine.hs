{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Engine
--
----------------------------------------------------------------------


module Cloud.Engine
    (
      -- * Basic Types to work with clouds
      BookmarkCloud

      -- * Construction
    , emptyBookmarkCloud

      -- * File system utilities
    , cloudFilePath
    , cloudDirectory

      -- * Environment manipulation
    , getConfig
    , getRootDir
    , getCurrentTime

      -- * Query cloud
    , isEmpty
    , search

      -- * Bookmark cloud manipulation
    , insertWith
    , update

      -- * Read/Write
    , readCloud
    , readCloudWithDefault
    , removeCloud
    , writeCloud
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Text           (pack)

import           App
import qualified Cloud.Config        as Cfg
import           Cloud.Core.Cloud    (Cloud)
import qualified Cloud.Core.Cloud    as Cloud
import           Cloud.Types


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text (Text)
-- >>> import           Cloud.Utils (integerToPOSIXMicroSeconds)
--
-- >>> let root = "/tmp"
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> e <- getDefaultAppEnvironment root
-- >>> let appRun = flip runApp e
-- >>> let insert = insertWith const
-- >>> let delete = update (const Nothing)
-- >>> let member e c = appRun (maybe False (==e) <$> search e c)
--
-- >>> data BM = BM { tu :: !Text, ttgs :: ![Text] } deriving (Show)
-- >>> instance Bookmarkable BM where { bookmarkTitle _ = ""; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tu; bookmarkTags = ttgs; bookmarkType _ = Book }
--
-- >>> data BT = BT { tgt :: !Text, tgtgs :: ![Text] } deriving (Show)
-- >>> instance Bookmarkable BT where { bookmarkTitle = tgt; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tgt; bookmarkTags = tgtgs; bookmarkType _ = Tag }
--
-- >>> let dcfg = Cfg.defaultConfig
-- >>> let fpath b = Cfg.cloudFilePath dcfg root b
-- >>> let ecloud b = Cloud.emptyCloud (bookmarkType b) (i2p 0) (i2p 0)
-- >>> let emptyBCloud b = BC(fpath b, ecloud b)


newtype BookmarkCloud = BC { getCloud :: (FilePath, Cloud) }

-- | Return 'True'if bookmark cloud is empty, 'False' otherwise.
--
-- test bookmark cloud
--
-- >>> let b1  = bookmarkableToBookmark (BM "u1" [])
--
-- >>> let bc1 = emptyBCloud b1
-- >>> isEmpty bc1
-- True
--
-- test tag cloud
--
-- >>> let t1 = bookmarkableToBookmark (BT "t1" [])
-- >>> let tc1 = emptyBCloud t1
-- >>> isEmpty tc1
-- True
--
isEmpty :: BookmarkCloud -> Bool
isEmpty (BC(_, c)) = Cloud.isEmpty c

-- | Search a bookmarkable in the cloud. Returns @('Just' v)@
-- if present, or 'Nothing' otherwise.
--
-- test bookmark cloud
--
-- >>> let b1  = bookmarkableToBookmark (BM "u1" [])
--
-- >>> let bc1 = emptyBCloud b1
-- >>> isEmpty bc1
-- True
--
-- >>> (Right bc2) <- appRun (insert b1 bc1)
-- >>> isEmpty bc2
-- False
--
-- test search
--
-- >>> (Right (Just b1')) <- appRun (search b1 bc2)
-- >>> b1' == b1
-- True
--
-- test tag cloud
--
-- >>> let t1 = bookmarkableToBookmark (BT "t1" [])
-- >>> let tc1 = emptyBCloud t1
-- >>> isEmpty tc1
-- True
--
-- >>> (Right tc2) <- appRun (insert b1 tc1)
-- >>> isEmpty tc2
-- True
-- >>> (Right tc3) <- appRun (insert t1 tc1)
-- >>> isEmpty tc3
-- False
--
-- test search
--
-- >>> (Right (Just t1')) <- appRun (search t1 tc3)
-- >>> t1' == t1
-- True
-- >>> (Right (x)) <- appRun (search b1 tc3)
-- >>> x == Nothing
-- True
--
search :: Bookmarkable b => b
       -> BookmarkCloud
       -> App (Maybe Bookmark)
search b c = do
    result <- performOp search' b c
    case result of
        Just r -> return r
        Nothing -> return Nothing
  where
      search' = Cloud.search $ bookmarkableToBookmark b

-- | Insert with a function, combining new value and old value.
-- @insertWith f b bookmarkcloud@ will insert b into bookmarkcloud
-- if b does not exist in the bookmarkcloud. Otherwise, the
-- function will insert the value @f new_value old_value@ into
-- the bookmarkcloud.
-- test bookmark cloud
--
-- >>> let b1  = bookmarkableToBookmark (BM "u1" [])
--
-- >>> let bc1 = emptyBCloud b1
-- >>> isEmpty bc1
-- True
--
-- >>> (Right bc2) <- appRun (insert b1 bc1)
-- >>> isEmpty bc2
-- False
--
-- test tag cloud
--
-- >>> let t1 = bookmarkableToBookmark (BT "t1" [])
-- >>> let tc1 = emptyBCloud t1
-- >>> isEmpty tc1
-- True
--
-- >>> (Right tc2) <- appRun (insert b1 tc1)
-- >>> isEmpty tc2
-- True
-- >>> (Right tc3) <- appRun (insert t1 tc1)
-- >>> isEmpty tc3
-- False
--
-- test modification
--
-- >>> let b2 = b1 & tags %~ (++["test-tag"])
-- >>> (Right bc3) <- appRun (insertWith (\_ _  -> b2) b1 bc2)
-- >>> isEmpty bc3
-- False
--
-- >>> (Right (Just b2')) <- appRun (search b2 bc3)
-- >>> b2' ^. tags == b2 ^. tags
-- True
--
insertWith :: Bookmarkable b => (Bookmark -> Bookmark -> Bookmark)
           -> b
           -> BookmarkCloud
           -> App BookmarkCloud
insertWith f b c = do
    path <- cloudFilePath b
    time <- getCurrentTime
    let bj = bookmarkableToBookmark b

    result <- performOp (Cloud.insertWith time f bj) b c
    case result of
        Just c' -> BC <$> return (path, c')
        Nothing -> return c

-- | The expression @(update f b cloud)@ updates the value @b@
-- (if it is in the cloud). If @(f b)@ is 'Nothing', the element
-- is deleted. If it is @('Just' y)@, then @b@ is replaced by @y@.
--
-- >>> let b1  = bookmarkableToBookmark (BM "u1" [])
-- >>> let c1 = emptyBCloud b1
-- >>> isEmpty c1
-- True
--
-- >>> (Right c2) <- appRun (insert b1 c1)
-- >>> isEmpty c2
-- False
--
-- >>> (Right c3) <- appRun (delete b1 c2)
-- >>> isEmpty c3
-- True
--
-- >>> let b2 = b1 & title .~ "test title"
-- >>> (Right c4) <- appRun (update (const (Just b2)) b1 c2)
-- >>> isEmpty c4
-- False
-- >>> (Right (Just b2')) <- appRun (search b2 c4)
-- >>> b2' ^. title == b2 ^. title
-- True
--
update :: Bookmarkable b => (Bookmark -> Maybe Bookmark)
       -> b
       -> BookmarkCloud
       -> App BookmarkCloud
update f b c = do
    path <- cloudFilePath b
    time <- getCurrentTime
    let bj = bookmarkableToBookmark b

    result <- performOp (Cloud.update time f bj) b c
    case result of
        Just c' -> BC <$> return (path, c')
        Nothing -> return c

-- | Read a cloud from disk. If the operation
-- fails it returns a default cloud.
readCloudWithDefault :: Bookmarkable b => b -> App BookmarkCloud
readCloudWithDefault b = f <$> emptyBookmarkCloud b <*> readCloud b
  where f = flip maybe id

-- | Read a cloud from disk.
readCloud :: Bookmarkable b => b -> App (Maybe BookmarkCloud)
readCloud b = do
    path <- cloudFilePath b
    js   <- readJSON path
    return $ BC <$> (path,) <$> js

-- | Remove the json file associate with the cloud.
removeCloud :: BookmarkCloud -> App ()
removeCloud (BC(path,_)) = removeJSON path

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

    go (path == path' && btype == c' ^. Cloud.cloudType)

emptyBookmarkCloud :: Bookmarkable b => b -> App BookmarkCloud
emptyBookmarkCloud b = do
    time <- getCurrentTime
    path <- cloudFilePath b

    return $ BC (path, Cloud.emptyCloud (bookmarkType b) time time)

-- | Returns the current 'Config' object
getConfig :: App Config
getConfig = config <$> currentEnvironment

-- | Returns the root directory of the cloud home.
getRootDir :: App FilePath
getRootDir = rootDir <$> currentEnvironment


-- | Returns current time.
getCurrentTime :: App POSIXMicroSeconds
getCurrentTime = currentTime <$> currentEnvironment

instance Eq BookmarkCloud where
  BC(_, c1) == BC(_, c2) = c1 == c2
