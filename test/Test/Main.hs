{-# LANGUAGE OverloadedStrings #-}

module Test.Main where

import Control.Lens
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text, append, pack)

import Control.Applicative
import qualified Cloud.Config as Cfg
import Api.Cloud
import App
import Cloud.Engine
import Api.Bookmark


cfg = Cfg.defaultConfig
root = "/tmp"
i2p = integerToPOSIXMicroSeconds

e = unsafePerformIO $ getDefaultAppEnvironment root
appRun = flip runApp e
--
insert :: Bookmarkable b => b -> BookmarkCloud -> App BookmarkCloud
insert b = insertWith const b

delete :: Bookmarkable b => b -> BookmarkCloud -> App BookmarkCloud
delete b = update (const Nothing) b

maybeWith :: Bookmarkable b => (Bookmark -> Bookmark -> Bool)
          -> b
          -> Maybe Bookmark
          -> Bool
maybeWith f b = maybe False (f(bookmarkableToBookmark b))

memberWith' f b bc = appRun (maybeWith f b <$> search b bc)

memberWith f b = fmap g . memberWith' f b
  where g = either (const False) id

member :: Bookmarkable b => b -> BookmarkCloud -> IO Bool
member = memberWith (==)
--
data BM = BM { tu :: !Text, ttgs :: ![Text] } deriving (Show)
--
instance Bookmarkable BM where { bookmarkTitle _ = ""; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tu; bookmarkTags = ttgs; bookmarkType _ = Book }
--
instance Eq BM where { (==) = (==) `on` tu }
--
data BT = BT { tgt :: !Text, tgtgs :: ![Text] } deriving (Show)
--
instance Eq BT where { (==) = (==) `on` tgt }
--
instance Bookmarkable BT where { bookmarkTitle = tgt; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tgt; bookmarkTags = tgtgs; bookmarkType _ = Tag }
--
i2t p i = append p $ pack $ show i

fpath :: Bookmarkable b => b -> FilePath
fpath b = Cfg.cloudFilePath cfg root b
--
mksampleBM i = BM (i2t "u" i) (map (i2t "t") [1..i])
mksamplesBM i = map mksampleBM [0..i]
mksampleBT i = BT (i2t "u" i) (map (i2t "t") [1..i])
mksamplesBT i = map mksampleBT [0..i]
sampleSize = 2 * maximumNumberOfClouds Cfg.defaultConfig
sampleBMs = mksamplesBM sampleSize
sampleBTs = mksamplesBT sampleSize
