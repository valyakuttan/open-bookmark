{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
----------------------------------------------------------------------
-- |
-- Module : BookmarkCloud.Core.Tag
--
----------------------------------------------------------------------


module BookmarkCloud.Core.Tag
    (
      Taggable (..)
    , TagCloud
    , TagJSON
    , emptyTagCloud
    , insertTag
    , lookupTag
    ) where


import           Control.Lens
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Function                (on)
import           Data.List                    ((\\))
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import           GHC.Generics

import           BookmarkCloud.Core.Taggable
import           BookmarkCloud.Data.DataStore (DataStore, Storable (..))
import qualified BookmarkCloud.Data.DataStore as DS
import           BookmarkCloud.Utils

-- $setup
--
-- >>> :set -XOverloadedStrings
--
-- >>> import qualified Data.Text as T
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> let lnks l = map (T.append "bookmarks-" . T.pack .show) [0..l]
-- >>> let mktag t n l = TagJSON t (i2p n) (i2p n) (lnks l)
-- >>> let testTags = [mktag "tag1" 1 2, mktag "tag2" 2 5]


data TagCloud = TagCloud {
      tagCloudCreated       :: !POSIXMicroSeconds
    , _tagCloudLastModified :: !POSIXMicroSeconds
    , _tagCloudTags         :: !(DataStore TagJSON)
    } deriving (Show, Generic)

data TagJSON = TagJSON {
      _title         :: !Text
    , _dateAdded     :: !POSIXMicroSeconds
    , _lastModified  :: !POSIXMicroSeconds
    , _bookmarkLinks :: ![Text]
    } deriving (Show, Generic)


makeLenses ''TagCloud
makeLenses ''TagJSON

-- | Lookup a tag in the cloud.
lookupTag :: Taggable t => t -> TagCloud -> Maybe TagJSON
lookupTag t c = DS.lookup (toTagJSON t) $ c ^. tagCloudTags

-- | Insert a tag into the cloud if not present.
-- Otherwise it will update bookmark links. Returns the
-- modified cloud.
--
-- >>> let ctime = i2p 5
-- >>> let i     = i2p 0
-- >>> let ts = emptyTagCloud i i
-- >>> let t1 = testTags !! 0
-- >>> let ts' = insertTag ctime t1 ts
-- >>> ts == ts'
-- False
-- >>> ts' ^. tagCloudLastModified == ctime
-- True
-- >>> DS.member t1 (ts' ^. tagCloudTags)
-- True
--
-- >>> let ctime' = i2p 6
-- >>> let ts'' = insertTag ctime' t1 ts'
-- >>> ts' == ts''
-- True
-- >>> ts'' ^. tagCloudLastModified == ctime
-- True
-- >>> DS.member t1 (ts' ^. tagCloudTags)
-- True
--
-- >>> let t1' = t1 & bookmarkLinks %~ (++ ["test link"])
-- >>> let ts3 = insertTag ctime' t1' ts'
-- >>> ts' == ts3
-- False
-- >>> ts3 ^. tagCloudLastModified == ctime'
-- True
-- >>> DS.member t1 (ts3 ^. tagCloudTags)
-- True
-- >>> DS.member t1' (ts3 ^. tagCloudTags)
-- True
-- >>> let (Just x) = DS.lookup t1' (ts3 ^. tagCloudTags)
-- >>> "test link" `elem` (x ^. bookmarkLinks)
-- True
--
-- >>> let ct7 = i2p 7
-- >>> let t2 = testTags !! 1
-- >>> let ts4 = insertTag ct7 t2 ts3
-- >>> DS.member t1 (ts4 ^. tagCloudTags)
-- True
-- >>> DS.member t2 (ts4 ^. tagCloudTags)
-- True
-- >>> ts4 ^. tagCloudLastModified == ct7
-- True
--
insertTag :: Taggable t => POSIXMicroSeconds
           -> t
           -> TagCloud
           -> TagCloud
insertTag currentTime t c = fromMaybe (ins tj) $ do
    x <- DS.lookup tj (c ^. tagCloudTags)

    let go diff
           | null diff = return c
           | otherwise = return $ ins x'
          where x' = x & bookmarkLinks %~ (++ diff) &
                     lastModified .~ currentTime

    go (tj ^. bookmarkLinks \\ x ^. bookmarkLinks)
  where
    ins a = c & tagCloudTags %~ DS.insert a &
            tagCloudLastModified .~ currentTime
    tj    = toTagJSON t

-- | Convert a 'Taggable' object to 'TagJSON'
toTagJSON :: Taggable t => t -> TagJSON
toTagJSON t = TagJSON (tagTitle t)
                      (tagDateAdded t)
                      (tagLastModified t)
                      (tagBookmarkLinks t)

-- | Construct an empty 'TagCloud'
emptyTagCloud :: POSIXMicroSeconds -- ^ Date of creation
           -> POSIXMicroSeconds    -- ^ Date of modification
           -> TagCloud             -- ^ 'TagCloud' created
emptyTagCloud d m = TagCloud
    { tagCloudCreated       = d
    , _tagCloudLastModified = m
    , _tagCloudTags         = DS.emptyStore
    }

instance Taggable TagJSON where
    tagTitle = view title
    tagDateAdded = view dateAdded
    tagLastModified = view lastModified
    tagBookmarkLinks = view bookmarkLinks

instance Ord TagJSON where
    compare = compare `on` _title

instance Eq TagJSON where
    (==) = (==) `on` _title

instance Eq TagCloud where
    (==) = (==) `on` f
      where
        f (TagCloud c m ts) = (ts, m, c)

instance FromJSON TagCloud
instance ToJSON TagCloud

instance FromJSON TagJSON
instance ToJSON TagJSON

instance Storable TagJSON where
    key = tagTitle
