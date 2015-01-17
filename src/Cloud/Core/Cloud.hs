{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Core.Cloud
--
----------------------------------------------------------------------


module Cloud.Core.Cloud
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
    , insertWith
    , update

      -- * Querying
    , isEmpty
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
-- >>> let member a = maybe False (== a) . search a
-- >>> let insert t = insertWith t const
-- >>> let delete t = update t (const Nothing)
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

-- | Construct an empty  'Cloud'.
emptyCloud :: BookmarkType      -- ^ Cloud type
           -> POSIXMicroSeconds -- ^ Date of creation
           -> POSIXMicroSeconds -- ^ Date of modification
           -> Cloud             -- ^ 'Cloud' created
emptyCloud t c m = Cloud (pack (show t)) c m DS.emptyStore

-- | Return 'True'if this Store is empty, 'False' otherwise.
--
-- >>> let i = i2p 0
-- >>> let cl0 = emptyCloud Book i i
-- >>> isEmpty cl0
-- True
isEmpty :: Cloud -> Bool
isEmpty c = DS.isEmpty $ c ^. cloudItems

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
search b c = DS.lookup b $ c ^. cloudItems

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
insertWith ctime f b c = insert'
  where
      insert' = c & cloudItems %~ DS.insertWith f b &
                cloudLastModified .~ ctime

-- | The expression @(update f b cloud)@ updates the value @b@
-- (if it is in the cloud). If @(f b)@ is 'Nothing', the element
-- is deleted. If it is @('Just' y)@, then @b@ is replaced by @y@.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts0 = emptyCloud Book i i
-- >>> let b1  = bookmarkableToBookmark (BM "t1" "u1")
-- >>> let ts1 = insert c5 b1 ts0
-- >>> ts1 == ts0
-- False
-- >>> isEmpty ts1
-- False
-- >>> member b1 ts1
-- True      
-- >>> ts1 ^. cloudLastModified == c5
-- True
-- >>> let (Just b1') = search b1 ts1
-- >>> b1' ^. title == b1 ^. title
-- True
-- >>> let c6 = i2p 6
-- >>> let ts2 = delete c6 b1 ts1
-- >>> member b1 ts2
-- False
-- >>> isEmpty ts2
-- True
-- >>> ts2 ^. cloudLastModified == c6
-- True
-- >>> let b2 = b1 & title .~ "test"
-- >>> let c7 = i2p 7
-- >>> let ts3 = update c7 (\_ -> Just b2) b1 ts1
-- >>> member b1 ts3
-- True
-- >>> member b2 ts3
-- True
-- >>> let (Just x) = search b2 ts3
-- >>> x ^. title == b2 ^. title
-- True
update :: POSIXMicroSeconds
       -> (Bookmark -> Maybe Bookmark)
       -> Bookmark
       -> Cloud
       -> Cloud
update ctime f b c = update'
  where
      update' = c & cloudItems %~ DS.update f b &
                cloudLastModified .~ ctime

instance FromJSON Cloud
instance ToJSON Cloud

instance Eq Cloud where
    (==) = (==) `on` f
      where
        f (Cloud t c m xs) = (t, xs, m, c)
