{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Core.Types
--
----------------------------------------------------------------------


module Cloud.Core.Types
    (
      -- * Basic types and constructors
      Bookmarkable (..)
    , Bookmark
    , BookmarkTag (..)
    , BookmarkType (..)
    , Storable (..)

      -- * Lenses to work with 'Bookmark'
    , title
    , dateAdded
    , lastModified
    , uri
    , tags

      -- * Smart Conversion to 'Bookmark'
    , bookmarkableToBookmark
    ) where


import           Control.Lens
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Function    (on)
import           GHC.Generics     (Generic)
import           Data.Text

import           Cloud.Utils



data BookmarkType = Book | Tag
                  deriving (Eq, Ord, Show)

class Bookmarkable b where
    bookmarkTitle        :: b -> Text
    bookmarkDateAdded    :: b -> POSIXMicroSeconds
    bookmarkLastModified :: b -> POSIXMicroSeconds
    bookmarkUri          :: b -> Text
    bookmarkTags         :: b -> [Text]
    bookmarkType         :: b -> BookmarkType
    bookmarkHash         :: b -> Int
    bookmarkHash         = textHash . bookmarkUri

class (Eq a, FromJSON a , ToJSON a) => Storable a where
    key :: a -> Text

type Tag = Text
type Url = Text

data Bookmark = Bookmark {
      _title        :: !Text
    , _dateAdded    :: !POSIXMicroSeconds
    , _lastModified :: !POSIXMicroSeconds
    , _uri          :: !Url
    , _tags         :: ![Tag]
    } deriving (Show, Generic)

makeLenses ''Bookmark

data BookmarkTag = BookmarkTag
    { tagTitle         :: !Text
    , tagDateAdded     :: !POSIXMicroSeconds
    , tagLastModified  :: !POSIXMicroSeconds
    , tagBookmarkLinks :: ![Text]
    } deriving (Eq, Ord, Show)

-- | Convert a 'Bookmarkable' to 'Bookmark'.
bookmarkableToBookmark :: Bookmarkable b => b -> Bookmark
bookmarkableToBookmark b =  Bookmark
    { _title        = bookmarkTitle b
    , _dateAdded    = bookmarkDateAdded b
    , _lastModified = bookmarkLastModified b
    , _uri          = bookmarkUri b
    , _tags         = bookmarkTags b
    }

instance Bookmarkable BookmarkTag where
    bookmarkTitle        = tagTitle
    bookmarkDateAdded    = tagDateAdded
    bookmarkLastModified = tagLastModified
    bookmarkUri          = tagTitle
    bookmarkTags         = tagBookmarkLinks
    bookmarkType _       = Tag

instance ToJSON Bookmark
instance FromJSON Bookmark

instance Ord Bookmark where
  compare = compare `on` _uri

instance Eq Bookmark where
  (==) = (==) `on` _uri

instance Storable Bookmark where
    key = view uri
