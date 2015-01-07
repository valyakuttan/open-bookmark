{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
    , BookmarkTag
    , BookmarkType (..)
    , Storable (..)

      -- * Lenses to work with 'Bookmark'
    , title
    , dateAdded
    , lastModified
    , uri
    , tags

      -- * Lenses to work with 'BookmarkTag'
    , tagTitle
    , tagDateAdded
    , tagLastModified
    , tagBookmarkLinks

      -- * Smart construcotrs to work with 'Bookmark' and BookmarkTag
    , bookmarkableToBookmark
    , emptyBookmark
    , emptyBookmarkTag
    ) where


import           Control.Lens
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Function (on)
import           Data.Text
import           GHC.Generics  (Generic)

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
    { _tagTitle         :: !Text
    , _tagDateAdded     :: !POSIXMicroSeconds
    , _tagLastModified  :: !POSIXMicroSeconds
    , _tagBookmarkLinks :: ![Text]
    } deriving (Eq, Ord, Show)

makeLenses ''BookmarkTag

-- | Create an empty Bookmark
emptyBookmark :: Bookmark
emptyBookmark = Bookmark "" t t "" []
  where t = integerToPOSIXMicroSeconds 0

-- | Create an empty BookmarkTag
emptyBookmarkTag :: BookmarkTag
emptyBookmarkTag = BookmarkTag "" t t []
  where t = integerToPOSIXMicroSeconds 0

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
    bookmarkTitle        = view tagTitle
    bookmarkDateAdded    = view tagDateAdded
    bookmarkLastModified = view tagLastModified
    bookmarkUri          = view tagTitle
    bookmarkTags         = view tagBookmarkLinks
    bookmarkType _       = Tag

instance Bookmarkable Bookmark where
    bookmarkTitle        = view title
    bookmarkDateAdded    = view dateAdded
    bookmarkLastModified = view lastModified
    bookmarkUri          = view uri
    bookmarkTags         = view tags
    bookmarkType _       = Book

instance ToJSON Bookmark
instance FromJSON Bookmark

instance Ord Bookmark where
  compare = compare `on` _uri

instance Eq Bookmark where
  (==) = (==) `on` _uri

instance Storable Bookmark where
    key = view uri
