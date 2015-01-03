{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
----------------------------------------------------------------------
-- |
-- Module : BookmarkCloud.Core.Bookmark
--
----------------------------------------------------------------------


module BookmarkCloud.Core.Bookmark
    (
      Bookmarkable (..)
    , BookmarkCloud
    , BookmarkJSON
    , emptyBookmarkCloud
    , insertBookmark
    , lookupBookmark
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Function                   (on)
import           Data.List                       ((\\))
import           Data.Maybe
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           BookmarkCloud.Core.Bookmarkable
import           BookmarkCloud.Data.DataStore    (DataStore, Storable (..))
import qualified BookmarkCloud.Data.DataStore    as DS
import           BookmarkCloud.Utils

-- $setup
--
-- >>> :set -XOverloadedStrings
--
-- >>> import qualified Data.Text as T
-- >>> import FirefoxBookmark
-- >>> import Data.Maybe (fromJust)
-- >>> let testBs = map (fromJust . toBookmarkJSON) sampleBookmarks
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> let strEq (BookmarkJSON t a m u ts) (BookmarkJSON t' a' m' u' ts') = t == t' && a == a' && m == m' && u == u' && ts == ts'


data BookmarkCloud = BookmarkCloud {
      bookmarkCloudCreated       :: !POSIXMicroSeconds
    , _bookmarkCloudLastModified :: !POSIXMicroSeconds
    , _bookmarkCloudBookmarks    :: !(DataStore BookmarkJSON)
    } deriving (Show, Generic)

type Tag = Text
type Url = Text

data BookmarkJSON = BookmarkJSON {
      _title        :: !Text
    , _dateAdded    :: !POSIXMicroSeconds
    , _lastModified :: !POSIXMicroSeconds
    , _uri          :: !Url
    , _tags         :: ![Tag]
    } deriving (Show, Generic)

makeLenses ''BookmarkCloud
makeLenses ''BookmarkJSON

-- | Lookup a url in the cloud.
lookupBookmark :: Bookmarkable b => b
               -> BookmarkCloud
               -> Maybe BookmarkJSON
lookupBookmark b c =
    toBookmarkJSON b >>= flip DS.lookup (c ^. bookmarkCloudBookmarks)

-- | Insert a bookmark into the cloud if not present.
-- Otherwise it will update tags. Returns the
-- modified cloud.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts0 = emptyBookmarkCloud i i
-- >>> let b1  = testBs !! 0
-- >>> let ts1 = insertBookmark c5 b1 ts0
-- >>> ts1 == ts0
-- False
-- >>> ts1 ^. bookmarkCloudLastModified == c5
-- True
-- >>> let (Just b1') = lookupBookmark b1 ts1
-- >>> b1' `strEq` b1
-- True
-- >>> let b2 = b1 & title .~ "test title" & tags %~ (++["test tag"])
-- >>> let c6 = i2p 6
-- >>> let ts2 = insertBookmark c6 b2 ts1
-- >>> let (Just b2') = lookupBookmark b2 ts2
-- >>> b2' ^. title == b1 ^. title
-- True
-- >>> b2' ^. dateAdded == b1 ^. dateAdded
-- True
-- >>> b2' ^. lastModified == c6
-- True
-- >>> b2' ^. uri == b1 ^. uri
-- True
-- >>> b2' ^. tags == (b1 ^. tags ++ ["test tag"])
-- True

insertBookmark :: Bookmarkable b => POSIXMicroSeconds
                -> b
                -> BookmarkCloud
                -> BookmarkCloud
insertBookmark currentTime b c = case b' of
    Nothing -> c
    Just bj -> fromMaybe (ins bj) $ do
        x <- DS.lookup bj (c ^. bookmarkCloudBookmarks)

        let go diff
                | null diff = return c
                | otherwise = return $ ins x'
              where x' = x & tags %~ (++ diff) &
                         lastModified .~ currentTime

        go (bj ^. tags \\ x ^. tags)
  where
      ins a = c & bookmarkCloudBookmarks %~ DS.insert a &
              bookmarkCloudLastModified .~ currentTime
      b'    = BookmarkJSON <$>
              fixdtitle <*>
              fixdate bookmarkDateAdded <*>
              fixdate bookmarkLastModified <*>
              bookmarkUri b <*>
              pure (bookmarkTags b)
      fixdtitle = bookmarkTitle b <|> pure "No title"
      fixdate x = x b <|> pure currentTime

-- | Construct an empty  'BookmarkCloud'
emptyBookmarkCloud :: POSIXMicroSeconds -- ^ Date of creation
              -> POSIXMicroSeconds -- ^ Date of modification
              -> BookmarkCloud     -- ^ 'BookmarkCloud' created
emptyBookmarkCloud c m = BookmarkCloud
    { bookmarkCloudCreated       = c
    , _bookmarkCloudLastModified = m
    , _bookmarkCloudBookmarks    = DS.emptyStore
    }

-- | A placeholder bookmark for a url.
toBookmarkJSON :: Bookmarkable b => b -> Maybe BookmarkJSON
toBookmarkJSON b = BookmarkJSON <$>
                   bookmarkTitle b <*>
                   bookmarkDateAdded b <*>
                   bookmarkLastModified b <*>
                   bookmarkUri b <*>
                   pure (bookmarkTags b)

instance Bookmarkable BookmarkJSON where
  bookmarkTitle        = Just . view title
  bookmarkDateAdded    = Just . view dateAdded
  bookmarkLastModified = Just . view lastModified
  bookmarkUri          = Just . view uri
  bookmarkTags         = view tags

instance ToJSON BookmarkJSON
instance FromJSON BookmarkJSON

instance FromJSON BookmarkCloud
instance ToJSON BookmarkCloud

instance Ord BookmarkJSON where
  compare = compare `on` _uri

instance Eq BookmarkCloud where
    (==) = (==) `on` f
      where
        f (BookmarkCloud c m xs) = (xs, m, c)

instance Eq BookmarkJSON where
  (==) = (==) `on` _uri

instance Storable BookmarkJSON where
    key = view uri
