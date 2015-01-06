{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Core.Engine
--
----------------------------------------------------------------------


module Cloud.Core.Engine
    (
      -- * Cloud Types
      Cloud
    , Bookmark

      -- * Lenses to work with 'Cloud'
    , cloudType
    , cloudCreated
    , cloudLastModified
    , cloudItems

      -- * Construction
    , emptyCloud

      -- * Modification
    , insert
    , insertWith

      -- * Querying
    , search

      -- * Conversion
    , bookmarkableToBookmark
     ) where


import           Control.Lens
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Function    (on)
import           Data.Text        (Text, pack)
import           GHC.Generics     (Generic)

import qualified Cloud.Core.Store as DS
import           Cloud.Core.Types
import           Cloud.Utils      (POSIXMicroSeconds)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import           Cloud.Utils (integerToPOSIXMicroSeconds)
--
-- >>> let i2p = integerToPOSIXMicroSeconds
--
-- >>> data BM = BM { tt :: !Text, tu :: !Text } deriving (Show)
-- >>> instance Bookmarkable BM where { bookmarkTitle = tt; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tu; bookmarkTags _ = []; bookmarkType _ = Book }


data Cloud = Cloud {
      _cloudType         :: !Text
    , _cloudCreated      :: !POSIXMicroSeconds
    , _cloudLastModified :: !POSIXMicroSeconds
    , _cloudItems        :: !(DS.Store Bookmark)
    } deriving (Show, Generic)

makeLenses ''Cloud

-- | Lookup an item in the cloud.
-- Returns @('Just' v) if found, or 'Nothing' otherwise.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts = emptyCloud Book i i
-- >>> let b1  = bookmarkableToBookmark (BM "t1" "u1")
-- >>> search b1 ts
-- Nothing
search :: Bookmark -> Cloud -> Maybe Bookmark
search b c = DS.lookup b (c ^. cloudItems)

-- | Insert with a function, combining new value and old value.
-- @insertWith f bookmark cloud@ will insert bookmark into cloud
-- if bookmark does not exist in the cloud. Otherwise, the
-- function will insert the value @f new_value old_value@ into
-- the cloud.
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts0 = emptyCloud Book i i
-- >>> let b1  = bookmarkableToBookmark (BM "t1" "u1")
-- >>> let ts1 = insert c5 b1 ts0
-- >>> ts1 == ts0
-- False
-- >>> ts1 ^. cloudLastModified == c5
-- True
-- >>> let (Just b1') = search b1 ts1
-- >>> b1' ^. title == b1 ^. title
-- True
-- >>> let c6 = i2p 6
-- >>> let b2 = b1 & tags %~ (++ ["test1", "test2"])
-- >>> let f a' a = a & tags %~ (++ a'^.tags)
-- >>> let ts2 = insertWith c6 f b2 ts1
-- >>> let (Just b2') = search b1 ts2
-- >>> b2' ^. tags == b2 ^. tags
-- True
-- >>> let (Just b2'') = search b2 ts2
-- >>> b2'' ^. tags == b2 ^. tags
-- True
insertWith :: POSIXMicroSeconds
           -> (Bookmark -> Bookmark -> Bookmark)
           -> Bookmark
           -> Cloud
           -> Cloud
insertWith ctime f b c = ins b
  where
      ins a = c & cloudItems %~ DS.insertWith f a &
              cloudLastModified .~ ctime

-- | Insert a new bookmark to the cloud.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts0 = emptyCloud Book i i
-- >>> let b1  = bookmarkableToBookmark (BM "t1" "u1")
-- >>> let ts1 = insert c5 b1 ts0
-- >>> ts1 == ts0
-- False
-- >>> ts1 ^. cloudLastModified == c5
-- True
-- >>> let (Just b1') = search b1 ts1
-- >>> b1' ^. title == b1 ^. title
-- True
insert :: POSIXMicroSeconds -> Bookmark -> Cloud -> Cloud
insert ctime = insertWith ctime const

-- | Construct an empty  'Cloud'.
emptyCloud :: BookmarkType      -- ^ Cloud type
           -> POSIXMicroSeconds -- ^ Date of creation
           -> POSIXMicroSeconds -- ^ Date of modification
           -> Cloud             -- ^ 'Cloud' created
emptyCloud t c m = Cloud (pack (show t)) c m DS.emptyStore

instance FromJSON Cloud
instance ToJSON Cloud

instance Eq Cloud where
    (==) = (==) `on` f
      where
        f (Cloud t c m xs) = (t, xs, m, c)
