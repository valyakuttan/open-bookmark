{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Cloud
--
----------------------------------------------------------------------


module Cloud.Cloud
    (
      -- * Cloud Types
      Cloud

      -- * Lenses to work with 'Cloud'
    , cloudCreated
    , cloudLastModified
    , store

      -- * Construction
    , emptyCloud

      -- * Modification
    , insertWith
    , update

      -- * Querying
    , isEmpty
    , search
    ) where


import           Control.Lens
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Function (on)
import           GHC.Generics  (Generic)

import           Cloud.Store   (Store)
import qualified Cloud.Store   as DS
import           Cloud.Types
import           Cloud.Utils   (POSIXMicroSeconds)


-- $setup
--
-- >>> import Cloud.Utils
--
-- >>> let i2p = integerToPOSIXMicroSeconds
--


data Cloud a = Cloud {
      _cloudCreated      :: !POSIXMicroSeconds
    , _cloudLastModified :: !POSIXMicroSeconds
    , _store             :: !(Store a)
    } deriving (Show, Generic)

makeLenses ''Cloud

instance Functor Cloud where
    fmap f (Cloud c l a) = Cloud c l $ fmap f a

instance Foldable Cloud where
    foldMap f (Cloud _ _ a) = foldMap f a

instance Traversable Cloud where
    traverse f c = (\s -> c & store .~ s) <$> traverse f (c ^. store)

-- | Construct an empty  'Cloud'.
emptyCloud :: Storable a => POSIXMicroSeconds -- ^ Date of creation
           -> POSIXMicroSeconds               -- ^ Date of modification
           -> Cloud a                         -- ^ 'Cloud' created
emptyCloud c m = Cloud c m DS.emptyStore

-- | Return 'True'if this Store is empty, 'False' otherwise.
--
-- >>> let i = i2p 0
-- >>> let cl0 = emptyCloud i i :: Cloud Bookmark
-- >>> isEmpty cl0
-- True
isEmpty :: Storable a => Cloud a -> Bool
isEmpty c = DS.isEmpty $ c ^. store

-- | Lookup an item in the cloud.
-- Returns @('Just' v) if found, or 'Nothing' otherwise.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts = emptyCloud i i :: Cloud Bookmark
-- >>> let b1  = emptyBookmark
-- >>> search b1 ts
-- Nothing
search :: Storable a => a -> Cloud a -> Maybe a
search b c = DS.lookup b $ c ^. store

-- | Insert with a function, combining new value and old value.
-- @insertWith f bookmark cloud@ will insert bookmark into cloud
-- if bookmark does not exist in the cloud. Otherwise, the
-- function will insert the value @f new_value old_value@ into
-- the cloud.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let insert t = insertWith t const
-- >>> let ts0 = emptyCloud i i :: Cloud Bookmark
-- >>> let b1  = emptyBookmark
-- >>> let ts1 = insert c5 b1 ts0
-- >>> let (Just b1') = search b1 ts1
-- >>> b1' == b1
-- True
--
insertWith :: Storable a => POSIXMicroSeconds
           -> (a -> a -> a)
           -> a
           -> Cloud a
           -> Cloud a
insertWith ctime f b c = insert'
  where
      insert' = c & store %~ DS.insertWith f b &
                cloudLastModified .~ ctime

-- | The expression @(update f b cloud)@ updates the value @b@
-- (if it is in the cloud). If @(f b)@ is 'Nothing', the element
-- is deleted. If it is @('Just' y)@, then @b@ is replaced by @y@.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let insert t = insertWith t const
-- >>> let delete t = update t $ const Nothing
-- >>> let ts0 = emptyCloud i i :: Cloud Bookmark
-- >>> let b1  = emptyBookmark
-- >>> let ts1 = insert c5 b1 ts0
-- >>> let c6 = i2p 6
-- >>> let ts2 = delete c6 b1 ts1
-- >>> search b1 ts2
-- Nothing
-- >>> isEmpty ts2
-- True
--
update :: Storable a => POSIXMicroSeconds
       -> (a -> Maybe a)
       -> a
       -> Cloud a
       -> Cloud a
update ctime f b c = update'
  where
      update' = c & store %~ DS.update f b &
                cloudLastModified .~ ctime

instance Storable a => FromJSON (Cloud a)
instance Storable a => ToJSON (Cloud a)

instance Eq a => Eq (Cloud a) where
    (==) = let f (Cloud c m xs) = (xs, m, c)
           in (==) `on` f
