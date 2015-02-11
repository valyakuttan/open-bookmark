{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Types
--
----------------------------------------------------------------------


module Cloud.Types
    (
      -- * Basic types and constructors
      Bookmark
    , Tag
    , Storable (..)
    , ToBookmark (..)

      -- * Lenses to work with 'Bookmark'
    , bookmarkTitle
    , bookmarkDateAdded
    , bookmarkLastModified
    , bookmarkUrl
    , bookmarkTags

      -- * Lenses to work with 'Tag'
    , tagTitle
    , tagDateAdded
    , tagLastModified
    , bookmarkUrls

      -- * Smart construcotrs to work with 'Bookmark' and BookmarkTag
    , emptyBookmark
    , emptyTag
    ) where


import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Function       (on)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

import           Cloud.Utils


class (Eq a, FromJSON a , ToJSON a) => Storable a where
    storeKey :: a -> Text

    storeType :: a -> String

    hash     :: a -> Int
    hash = textHash . storeKey

type Tg = Text
type Url = Text

data Bookmark = Bookmark {
      _bookmarkTitle        :: !Text
    , _bookmarkDateAdded    :: !POSIXMicroSeconds
    , _bookmarkLastModified :: !POSIXMicroSeconds
    , _bookmarkUrl          :: !Url
    , _bookmarkTags         :: ![Tg]
    } deriving (Show, Generic)

makeLenses ''Bookmark

data Tag = Tag
    { _tagTitle        :: !Text
    , _tagDateAdded    :: !POSIXMicroSeconds
    , _tagLastModified :: !POSIXMicroSeconds
    , _bookmarkUrls    :: ![Text]
    } deriving (Show, Generic)

makeLenses ''Tag

class ToBookmark b where
    toBookmark :: b -> Bookmark

-- | Create an empty bookmark
emptyBookmark :: Bookmark
emptyBookmark = Bookmark "" t t "" []
  where t = integerToPOSIXMicroSeconds 0

-- | Create an empty tag
emptyTag :: Tag
emptyTag = Tag "" t t []
  where t = integerToPOSIXMicroSeconds 0

instance ToJSON Bookmark
instance FromJSON Bookmark

instance ToJSON Tag
instance FromJSON Tag

instance Eq Tag where
    (==) = (==) `on` view tagTitle

instance Ord Tag where
    compare = compare `on` view tagTitle

instance Eq Bookmark where
    (==) = (==) `on` view bookmarkUrl

instance Ord Bookmark where
    compare = compare `on`view bookmarkUrl

instance Storable Bookmark where
    storeKey = view bookmarkUrl

    storeType _ = "book"

instance Storable Tag where
    storeKey = view tagTitle

    storeType _ = "tag"
