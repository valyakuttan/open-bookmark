{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Data.Store
--
----------------------------------------------------------------------


module Cloud.Core.Store
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
import           Control.Monad
import           Data.Aeson
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Text           (Text)
import           Prelude             hiding (lookup)


import           Cloud.Core.Types    (Storable (..))

-- $setup
--
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
--
-- >>> import Data.Function (on)
-- >>> import qualified Data.Text as T
-- >>> import GHC.Generics
--
-- >>> import Cloud.Utils
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
-- >>> let member a = maybe False (==a) . lookup a
-- >>> let insert = insertWith const
-- >>> let delete = update (const Nothing) 

type Hash1 = Text

type MapL1 a = Map Hash1 a

data Store a = Store { store :: !(MapL1 a) }
                   deriving (Show, Eq)

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
-- as (Just value), or Nothing if the key is not in the map.
lookup :: Storable a => a -> Store a -> Maybe a
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
singleton :: Storable a => a -> Store a
singleton = Store . singletonL1

-- |  Insert with a function, combining new value and old value.
-- @insertWith f a store@ will insert a into store if a does not
-- exist in the store. If a does exist, the function will insert
-- the value @f new_value old_value@ into the store.
insertWith :: Storable a => (a -> a -> a)
           -> a
           -> Store a
           -> Store a
insertWith f a = Store . insertWithL1 f a . store

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
-- >>> let s' = a1 `delete` s
-- >>> a1 `member` s'
-- False
-- >>> store s' == store s
-- True
--
-- >>> let s1 = insert a1 s
-- >>> isEmpty s1
-- False
-- >>> a1 `member` s1
-- True
--
-- >>> let a2 = mkTmpB "hello" 1 3 "world!!"
-- >>> a2 `member` s
-- False
--
-- >>> let s3 = a2 `delete` s1
-- >>> a2 `member` s3
-- False
--
-- >>> a1 `member` s3
-- True
-- >>> store s3 == store s1
-- True
--
-- >>> let s4 = a1 `delete` s3
-- >>> a2 `member` s3
-- False
-- >>> a1 `member` s4
-- False
-- >>> store s4 == store s
-- True
--
-- >>> let a3 = mkTmpB "hello!!!" 1 3 "world"
-- >>> let s5 = update  (\_ -> Just a3) a1 s1
-- >>> isEmpty s5
-- False
-- >>> a3 `member` s5
-- True
--
-- >>> let s6 = update (\_ -> Nothing) a3 s5
-- >>> a3 `member` s6
-- False
-- >>> isEmpty s6
-- True
update :: Storable a => (a -> Maybe a) -> a -> Store a -> Store a
update f a = Store . updateL1 f a . store

-- |  Return all elements of the store.
elems :: Storable a => Store a -> [a]
elems (Store s) = M.elems s

-- | Build a store from a list.
fromList :: Storable a => [a] -> Store a
fromList = foldr (insertWith const) emptyStore

updateL1 :: Storable a => (a -> Maybe a) -> a -> MapL1 a -> MapL1 a
updateL1 f a = M.update f (key a)

insertWithL1 :: Storable a => (a -> a -> a) -> a -> MapL1 a -> MapL1 a
insertWithL1 f a = M.insertWith f (key a) a

singletonL1 :: Storable a => a -> MapL1 a
singletonL1 a = M.singleton (key a) a

instance FromJSON a => FromJSON (Store a) where
    parseJSON (Object v) = Store <$> v .: "store"
    parseJSON _          = mzero

instance ToJSON a => ToJSON (Store a) where
    toJSON (Store s) = object ["store" .= s]
