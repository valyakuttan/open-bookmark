{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : BookmarkCloud.Data.DataStore
--
----------------------------------------------------------------------


module BookmarkCloud.Data.DataStore
    (
      -- * The @DataStore@ type
      DataStore
    , Storable (..)

      -- * Construction
    , emptyStore
    , singleton

      -- * Query
    , isEmpty
    , lookup
    , member

      -- * Modification
    , insert
    , insertWith
    , delete
    , update

      -- * Conversion
    , elems
    , fromList
    ) where


import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Text           (Text)
import           Prelude             hiding (lookup)


-- $setup
--
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
--
-- >>> import Data.Function (on)
-- >>> import qualified Data.Text as T
-- >>> import GHC.Generics
--
-- >>> import BookmarkCloud.Utils
--
-- >>> data TmpB = TmpB { tt :: !Text, td :: !POSIXMicroSeconds, tm :: !POSIXMicroSeconds, tu :: !Text } deriving (Show, Generic)
--
-- >>> instance Eq TmpB where { (==) = (==) `on` tu }
-- >>> instance Ord TmpB where { compare = compare `on` tu }
--
-- >>> let f  = T.pack . show
-- >>> instance FromJSON TmpB
-- >>> instance ToJSON TmpB
-- >>> instance Storable TmpB where { key = f . tu }
--
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> let mkTmpB t d m u = TmpB t (i2p d) (i2p m) u
--


type Hash1 = Text

type MapL1 a = Map Hash1 a

data DataStore a = DataStore { store :: !(MapL1 a) }
                   deriving (Show, Eq)

class (Eq a, FromJSON a , ToJSON a) => Storable a where
    key :: a -> Hash1


-- | Construct an empty DataStore.
--
emptyStore :: DataStore a
emptyStore = DataStore M.empty

-- | Return 'True'if this Store is empty, 'False' otherwise.
--
-- >>> isEmpty emptyStore
-- True
isEmpty :: DataStore a -> Bool
isEmpty = M.null . store

-- | Is the item a member of the store.
--
-- >>> let a = mkTmpB "hello" 1 2 "world"
-- >>> a `member` emptyStore
-- False
member :: Storable a => a -> DataStore a -> Bool
member a = maybe False (a ==) . lookup a

-- | Lookup the value a in the store.
-- The function will return the corresponding value
-- as (Just value), or Nothing if the key is not in the map.
lookup :: Storable a => a -> DataStore a -> Maybe a
lookup a = M.lookup (key a) . store
-- | A store with a single element.
--
-- >>> let a1 = mkTmpB "hello" 1 2 "world"
-- >>> let s = singleton a1
-- >>> isEmpty s
-- False
-- >>> a1 `member` s
-- True
-- >>> let a2 = mkTmpB "hello" 1 2 "world!"
-- >>> a2 `member` s
-- False
singleton :: Storable a => a -> DataStore a
singleton = DataStore . singletonL1

-- | Insert a new value to the Store.
--
-- >>> let a1 = mkTmpB "hello" 1 2 "world"
-- >>> let s = emptyStore
-- >>> isEmpty s
-- True
-- >>> a1 `member` s
-- False
-- >>> let a2 = mkTmpB "hello" 1 3 "world!!"
-- >>> a2 `member` s
-- False
-- >>> let s1 = insert a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
-- >>> let s2 = insert a2 s1
-- >>> isEmpty s2
-- False
-- >>> a1 `member` s2
-- True
-- >>> a2 `member` s2
-- True
--
-- >>> let a3 = a1 { tt = "test title" }
-- >>> let s3 = insert a3 s2
-- >>> a3 `member` s3
-- True
-- >>> let (Just a3') = lookup a3 s3
-- >>> tt a3' == tt a3
-- True
insert :: Storable a => a -> DataStore a -> DataStore a
insert a = DataStore . insertL1 a . store

-- |  Insert with a function, combining new value and old value.
-- @insertWith f a store@ will insert a into store if a does not
-- exist in the store. If a does exist, the function will insert
-- the value @f new_value old_value@ into the store.
insertWith :: Storable a => (a -> a -> a)
           -> a
           -> DataStore a
           -> DataStore a
insertWith f a = DataStore . insertWithL1 f a . store

-- | Delete a value from the store. When value is not in the
-- store, the original store is returned.
--
-- >>> let a1 = mkTmpB "hello" 1 2 "world"
-- >>> let s = emptyStore
-- >>> isEmpty s
-- True
-- >>> a1 `member` s
-- False
--
-- >>> let s' = a1 `delete` s
-- >>> a1 `member` s'
-- False
-- >>> store s' == store s
-- True
--
-- >>> let a2 = mkTmpB "hello" 1 3 "world!!"
-- >>> a2 `member` s
-- False
-- >>> let s1 = insert a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
-- >>> let s2 = insert a2 s1
-- >>> isEmpty s2
-- False
-- >>> a1 `member` s2
-- True
-- >>> a2 `member` s2
-- True
--
-- >>> let s3 = a2 `delete` s2
-- >>> a2 `member` s3
-- False
-- >>> a1 `member` s3
-- True
-- >>> store s3 == store s1
-- True
-- >>> let s4 = a1 `delete` s3
-- >>> a2 `member` s3
-- False
-- >>> a1 `member` s4
-- False
-- >>> store s4 == store s
-- True
delete :: Storable a => a -> DataStore a -> DataStore a
delete a = DataStore . deleteL1 a . store

-- | The expression (update f a s) updates the value a (if it is
-- in the store). If (f a) is Nothing, the element is deleted. If
-- it is (Just y), then a is replaced by y.
--
-- >>> let a1 = mkTmpB "hello" 1 2 "world"
-- >>> let s = emptyStore
-- >>> isEmpty s
-- True
-- >>> a1 `member` s
-- False
--
-- >>> let s1 = insert a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
-- >>> let a2 = mkTmpB "hello!!" 1 3 "world"
-- >>> let s2 = update  (\_ -> Just a2) a2 s1
-- >>> isEmpty s2
-- False
-- >>> a2 `member` s2
-- True
--
-- >>> let s3 = update (\_ -> Nothing) a2 s2
-- >>> a2 `member` s3
-- False
update :: Storable a => (a -> Maybe a) -> a -> DataStore a -> DataStore a
update f a s = case lookup a s of
                   Just x  -> case f x of
                                  Just x' -> insert x' s
                                  Nothing -> delete x s
                   Nothing -> s

-- |  Return all elements of the store.
elems :: Storable a => DataStore a -> [a]
elems (DataStore s) = M.elems s

-- | Build a store from a list.
fromList :: Storable a => [a] -> DataStore a
fromList = foldr insert emptyStore

deleteL1 :: Storable a => a -> MapL1 a -> MapL1 a
deleteL1 a = M.delete (key a)

insertL1 :: Storable a => a -> MapL1 a -> MapL1 a
insertL1 a = M.insert (key a) a

insertWithL1 :: Storable a => (a -> a -> a) -> a -> MapL1 a -> MapL1 a
insertWithL1 f a = M.insertWith f (key a) a

singletonL1 :: Storable a => a -> MapL1 a
singletonL1 a = M.singleton (key a) a

instance FromJSON a => FromJSON (DataStore a) where
    parseJSON (Object v) = DataStore <$> v .: "store"
    parseJSON _          = mzero

instance ToJSON a => ToJSON (DataStore a) where
    toJSON (DataStore s) = object ["store" .= s]
