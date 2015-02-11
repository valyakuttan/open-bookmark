{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Store
--
----------------------------------------------------------------------


module Cloud.Store
    (
      -- * The @Store@ type
      Store

      -- * Construction
    , emptyStore
    , singleton

      -- * Query
    , isEmpty
    , lookup

      -- * Modification
    , insertWith
    , update

      -- * Conversion
    , elems
    , fromList
    ) where


import           Control.Applicative
import           Data.Aeson
import           Data.Foldable       hiding (foldr)
import           Data.HashMap.Lazy   (HashMap)
import qualified Data.HashMap.Lazy   as M
import           Data.Maybe
import           Data.Text           (Text)
import           Data.Traversable
import           Prelude             hiding (lookup)

import           Cloud.Types         (Storable (..))


-- $setup
--
-- >>> :set -XOverloadedStrings
--
-- >>> import Control.Lens
-- >>> import Cloud.Utils
-- >>> import Cloud.Types
--


type Hash1 = Text

type MapL1 a = HashMap Hash1 a

data Store a = Store { store :: !(MapL1 a) }
                   deriving (Show, Eq)

instance Functor Store where
    fmap f (Store a) = Store $ fmap f a

instance Foldable Store where
   foldMap f (Store s) = foldMap f s

instance Traversable Store where
    traverse f (Store s) = Store <$> traverse f s

-- | Construct an empty Store.
--
emptyStore :: Store a
emptyStore = Store M.empty

-- | Return 'True'if this Store is empty, 'False' otherwise.
--
-- >>> isEmpty emptyStore
-- True
isEmpty :: Store a -> Bool
isEmpty = M.null . store

-- | Lookup the value a in the store.
-- The function will return the corresponding value
-- as (Just value), or Nothing if the storeKey is not in the map.
lookup :: Storable a => a -> Store a -> Maybe a
lookup a = M.lookup (storeKey a) . store
-- | A store with a single element.
--
-- >>> let member a s = maybe False (== a) $ lookup a s
-- >>> let a1 = emptyBookmark
-- >>> let s = singleton a1
-- >>> isEmpty s
-- False
-- >>> a1 `member` s
-- True
-- >>> let a2 = emptyBookmark & bookmarkUrl .~ "u1"
-- >>> a2 `member` s
-- False
singleton :: Storable a => a -> Store a
singleton = Store . singletonL1

-- |  Insert with a function, combining new value and old value.
-- @insertWith f a store@ will insert a into store if a does not
-- exist in the store. If a does exist, the function will insert
-- the value @f new_value old_value@ into the store.
--
-- >>> let member a s = maybe False (== a) $ lookup a s
-- >>> let a1 = emptyBookmark
-- >>> let s = emptyStore
-- >>> isEmpty s
-- True
-- >>> a1 `member` s
-- False
--
-- >>> let s1 = insertWith const a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
--
insertWith :: Storable a => (a -> a -> a)
           -> a
           -> Store a
           -> Store a
insertWith f a = Store . insertWithL1 f a . store

-- | The expression (update f a s) updates the value a (if it is
-- in the store). If (f a) is Nothing, the element is deleted. If
-- it is (Just y), then a is replaced by y.
--
-- >>> let member a s = maybe False (== a) $ lookup a s
-- >>> let a1 = emptyBookmark
-- >>> let s = emptyStore
-- >>> isEmpty s
-- True
-- >>> a1 `member` s
-- False
--
-- >>> let s1 = insertWith const a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
--
-- >>> let s2 = update (const Nothing) a1 s1
-- >>> isEmpty s2
-- True
-- >>> a1 `member` s2
-- False
--
update :: Storable a => (a -> Maybe a) -> a -> Store a -> Store a
update f a = Store . updateL1 f a . store

-- |  Return all elements of the store.
elems :: Storable a => Store a -> [a]
elems (Store s) = M.elems s

-- | Build a store from a list.
fromList :: Storable a => [a] -> Store a
fromList = foldr (insertWith const) emptyStore

updateL1 :: Storable a => (a -> Maybe a) -> a -> MapL1 a -> MapL1 a
updateL1 f a = case f a of
    Nothing -> M.delete (storeKey a)
    Just a' -> M.adjust (const a') (storeKey a)

insertWithL1 :: Storable a => (a -> a -> a) -> a -> MapL1 a -> MapL1 a
insertWithL1 f a = M.insertWith f (storeKey a) a

singletonL1 :: Storable a => a -> MapL1 a
singletonL1 a = M.singleton (storeKey a) a

instance FromJSON a => FromJSON (Store a) where
    parseJSON (Object v) = Store <$> v .: "store"
    parseJSON _          = empty

instance ToJSON a => ToJSON (Store a) where
    toJSON (Store s) = object ["store" .= s]
