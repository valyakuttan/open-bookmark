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
    , JSONData

      -- * Construction
    , emptyCloud

      -- * Modification
    , insert

      -- * Querying
    , search
    , cloudCreated

      -- * Conversion
    , bookmarkableToJSONData
    , taggableToJSONData
    ) where


import           Control.Lens
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Function           (on)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

import           Cloud.Core.Bookmarkable
import           Cloud.Core.DataStore    (DataStore, Storable (..))
import qualified Cloud.Core.DataStore    as DS
import           Cloud.Core.Taggable
import           Cloud.Utils

-- $setup
--
-- >>> :set -XOverloadedStrings
--
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> let strEq (JSONData t a m u ts) (JSONData t' a' m' u' ts') = t == t' && a == a' && m == m' && u == u' && ts == ts'


data Cloud = Cloud {
      cloudCreated       :: !POSIXMicroSeconds
    , _cloudLastModified :: !POSIXMicroSeconds
    , _cloudItems        :: !(DataStore JSONData)
    } deriving (Show, Generic)

type Tag = Text
type Url = Text

data JSONData = JSONData {
      _title        :: !Text
    , _dateAdded    :: !POSIXMicroSeconds
    , _lastModified :: !POSIXMicroSeconds
    , _uri          :: !Url
    , _tags         :: ![Tag]
    } deriving (Show, Generic)

makeLenses ''Cloud
makeLenses ''JSONData

-- | Lookup an item in the cloud.
-- Returns @('Just' v) if found, or 'Nothing' otherwise.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts = emptyCloud i i
-- >>> let b1  = JSONData "t" i i "u" []
-- >>> search b1 ts
-- Nothing
search :: JSONData -> Cloud -> Maybe JSONData
search b c = DS.lookup b (c ^. cloudItems)

-- | Insert a new item to the cloud.
--
-- >>> let c5 = i2p 5
-- >>> let i  = i2p 0
-- >>> let ts0 = emptyCloud i i
-- >>> let b1  = JSONData "t" i i "u" []
-- >>> let ts1 = insert c5 b1 ts0
-- >>> ts1 == ts0
-- False
-- >>> ts1 ^. cloudLastModified == c5
-- True
-- >>> let (Just b1') = search b1 ts1
-- >>> b1' `strEq` b1
-- True
insert :: POSIXMicroSeconds -> JSONData -> Cloud -> Cloud
insert currentTime b c = ins b
  where
      ins a = c & cloudItems %~ DS.insert a &
              cloudLastModified .~ currentTime

-- | Convert a 'Taggable' to 'JSONData'.
taggableToJSONData :: Taggable t => t -> JSONData
taggableToJSONData t = JSONData
    { _title        = tagTitle t
    , _dateAdded    = tagDateAdded t
    , _lastModified = tagLastModified t
    , _uri          = tagTitle t
    , _tags         = tagBookmarkLinks t
    }

-- | Convert a 'Bookmarkable' to 'JSONData'.
bookmarkableToJSONData :: Bookmarkable b => b -> JSONData
bookmarkableToJSONData b =  JSONData
    { _title        = bookmarkTitle b
    , _dateAdded    = bookmarkDateAdded b
    , _lastModified = bookmarkLastModified b
    , _uri          = bookmarkUri b
    , _tags         = bookmarkTags b
    }

-- | Construct an empty  'Cloud'.
emptyCloud :: POSIXMicroSeconds    -- ^ Date of creation
              -> POSIXMicroSeconds -- ^ Date of modification
              -> Cloud             -- ^ 'Cloud' created
emptyCloud c m = Cloud c m DS.emptyStore

instance Bookmarkable JSONData where
    bookmarkTitle        = view title
    bookmarkDateAdded    = view dateAdded
    bookmarkLastModified = view lastModified
    bookmarkUri          = view uri
    bookmarkTags         = view tags

instance Taggable JSONData where
    tagTitle         = view title
    tagDateAdded     = view dateAdded
    tagLastModified  = view lastModified
    tagBookmarkLinks = view tags

instance ToJSON JSONData
instance FromJSON JSONData

instance FromJSON Cloud
instance ToJSON Cloud

instance Ord JSONData where
  compare = compare `on` _uri

instance Eq Cloud where
    (==) = (==) `on` f
      where
        f (Cloud c m xs) = (xs, m, c)

instance Eq JSONData where
  (==) = (==) `on` _uri

instance Storable JSONData where
    key = view uri
