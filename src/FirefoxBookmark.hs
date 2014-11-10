{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module FirefoxBookmark (
      jsonToBookmarks
    , sampleBookmarks 
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text, split)
import GHC.Generics (Generic)

import Bookmark (Bookmarkable (..))
import Utils (integerToPOSIXMicroSeconds)


data RootMenu = RootMenu ![BookmarkMenu]
              deriving (Show, Generic)

data BookmarkMenu = BookmarkMenu !Text ![FirefoxBookmark]
                  deriving (Show, Generic)

data FirefoxBookmark = F {
      fbmTitle :: !(Maybe Text)
    , fbmDateAdded :: !Integer
    , fbmLastModified :: !Integer
    , fbmUri :: !(Maybe Text)
    , fbmTags :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Convert firefox bookmark json to @[Bookmark]@
-- bad json file will produce an error string
jsonToBookmarks :: ByteString -> Either String [FirefoxBookmark]
jsonToBookmarks = fmap getBookmarks . eitherDecode

-- | Retrieve all bookmarks from firefox @Bookmark menu@
-- firefox specific bookmarks will be removed from the list
getBookmarks :: RootMenu -> [FirefoxBookmark]
getBookmarks (RootMenu ms) = filter p $ concatMap firefoxBookmarks ms
  where
    p b = maybe False (`notElem` firefoxDefaults) $ fbmTitle b
    firefoxDefaults = [ "Recently Bookmarked"
                      , "Recent Tags"
                      , "Mozilla Firefox"
                      , "Most Visited"
                      , "Getting Started"
                      ]

-- | extract bookmarks from a menu item
firefoxBookmarks :: BookmarkMenu -> [FirefoxBookmark]
firefoxBookmarks (BookmarkMenu _ bs) = bs

instance Bookmarkable FirefoxBookmark where
  bookmarkTitle        = fromMaybe "" . fbmTitle
  bookmarkDateAdded    = integerToPOSIXMicroSeconds . fbmDateAdded
  bookmarkLastModified = integerToPOSIXMicroSeconds . fbmLastModified
  bookmarkUri          = fromMaybe "" . fbmUri
  bookmarkTags         = let splitText = split (== ',')
                         in maybe [] splitText . fbmTags

instance FromJSON RootMenu where
  parseJSON (Object v) = RootMenu <$> v .: "children"
  parseJSON _          = mzero

instance FromJSON BookmarkMenu where
  parseJSON (Object v) = BookmarkMenu <$> v .: "title" <*> v .: "children"
  parseJSON _          = mzero

instance FromJSON FirefoxBookmark where
  parseJSON (Object v) =
           F <$> v .:? "title"
             <*> v .: "dateAdded"
             <*> v .: "lastModified"
             <*> v .:? "uri"
             <*> v .:? "tags"
  parseJSON _ = mzero

sampleBookmarks :: [FirefoxBookmark]
sampleBookmarks = [F {fbmTitle = Just "Chapter\160\&15.\160Programming with monads", fbmDateAdded = 1414822121270582, fbmLastModified = 1414822121289726, fbmUri = Just "http://book.realworldhaskell.org/read/programming-with-monads.html", fbmTags = Just "haskell,read-it-later"},F {fbmTitle = Just "Real World Haskell", fbmDateAdded = 1414822172766037, fbmLastModified = 1414822173848154, fbmUri = Just "http://book.realworldhaskell.org/", fbmTags = Just "haskell,read-it-later,computer-science"}]
