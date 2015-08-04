{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Client.Firefox
--
----------------------------------------------------------------------


module Client.Firefox
    (
      jsonToBookmarks
    , sampleBookmarks
    ) where


import           Control.Lens
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.Text            (Text, split)
import           GHC.Generics         (Generic)

import           Api.Cloud



data RootMenu = RootMenu ![BookmarkMenu]
              deriving (Show, Generic)

data BookmarkMenu = BookmarkMenu !Text ![FirefoxBookmark]
                  deriving (Show, Generic)

data FirefoxBookmark = F {
      fbmTitle        :: !(Maybe Text)
    , fbmDateAdded    :: !(Maybe Integer)
    , fbmLastModified :: !(Maybe Integer)
    , fbmUri          :: !(Maybe Text)
    , fbmTags         :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Retrieve all bookmarks from firefox @Bookmark menu@
-- firefox specific bookmarks will be removed from the list
jsonToBookmarks :: ByteString -> Either String [FirefoxBookmark]
jsonToBookmarks = fmap getBookmarks . eitherDecode
  where
    getBookmarks (RootMenu ms) = filter p $ concatMap firefoxBookmarks ms
    p b = isJust (fbmUri b) &&
          maybe True (`notElem` firefoxDefaults) (fbmTitle b)
    firefoxDefaults = [ "Recently Bookmarked"
                      , "Recent Tags"
                      , "Mozilla Firefox"
                      , "Most Visited"
                      , "Getting Started"
                      ]

-- | extract bookmarks from a menu item
firefoxBookmarks :: BookmarkMenu -> [FirefoxBookmark]
firefoxBookmarks (BookmarkMenu _ bs) = bs

instance ToBookmark FirefoxBookmark where
  toBookmark b =
      emptyBookmark &
      bookmarkTitle .~ fromMaybe "Mozilla Firefox" (fbmTitle b) &
      bookmarkDateAdded .~ toPOSIX (fbmDateAdded b) &
      bookmarkLastModified .~ toPOSIX (fbmLastModified b) &
      bookmarkUrl .~ fromJust (fbmUri b) &
      bookmarkTags .~ maybe [] csvToList (fbmTags b)

toPOSIX :: Maybe Integer -> POSIXMicroSeconds
toPOSIX = maybe t integerToPOSIXMicroSeconds
  where t = integerToPOSIXMicroSeconds 1414822173848154

csvToList :: Text -> [Text]
csvToList = split (== ',')

instance FromJSON RootMenu where
  parseJSON (Object v) = RootMenu <$> v .: "children"
  parseJSON _          = mzero

instance FromJSON BookmarkMenu where
  parseJSON (Object v) = BookmarkMenu <$> v .: "title" <*> v .: "children"
  parseJSON _          = mzero

instance FromJSON FirefoxBookmark where
  parseJSON (Object v) =
           F <$> v .:? "title"
             <*> v .:? "dateAdded"
             <*> v .:? "lastModified"
             <*> v .:? "uri"
             <*> v .:? "tags"
  parseJSON _ = mzero

sampleBookmarks :: [FirefoxBookmark]
sampleBookmarks =
  [ F {
        fbmTitle = Just "Chapter\160\&15.\160Programming with monads"
     ,  fbmDateAdded = Just 1414822121270582
     ,  fbmLastModified = Just 1414822121289726
     ,  fbmUri = Just "http://book.realworldhaskell.org/read/programming-with-monads.html"
     ,  fbmTags = Just "haskell,read-it-later"
     }
  , F {
        fbmTitle = Just "Real World Haskell"
      , fbmDateAdded = Just 1414822172766037
      , fbmLastModified = Just 1414822173848154
      , fbmUri = Just "http://book.realworldhaskell.org/"
      , fbmTags = Just "haskell,read-it-later,computer-science"
      }
  ]
