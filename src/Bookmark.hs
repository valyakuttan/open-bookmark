{-# LANGUAGE OverloadedStrings, DeriveGeneric
            , TemplateHaskell, FlexibleContexts
            , FlexibleInstances #-}

module Bookmark (
      Bookmarkable (..)
    , BookmarkCloud
    , BookmarkJSON
    , bookmarkCloudCreated
    , defaultBookmarkCloud
    , insertBookmarks
    , makeBookmark
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.State (execState)
import Data.Function (on)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (nub)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics (Generic)

import Utils

class Bookmarkable b where
  bookmarkTitle        :: b -> Text
  bookmarkDateAdded    :: b -> POSIXMicroSeconds
  bookmarkLastModified :: b -> POSIXMicroSeconds
  bookmarkUri          :: b -> Text
  bookmarkTags         :: b -> [Text]
  bookmarkHash         :: b -> Int
  bookmarkHash         = textHash . bookmarkUri
  
data BookmarkCloud = BookmarkCloud {
      bookmarkCloudCreated :: !POSIXMicroSeconds
    , _bookmarkCloudLastModified :: !POSIXMicroSeconds
    , _bookmarkCloudBookmarks :: ![BookmarkJSON]
    } deriving (Show, Generic)

data BookmarkJSON = BookmarkJSON {
      _title  :: !Text
    , _dateAdded :: !POSIXMicroSeconds
    , _lastModified :: !POSIXMicroSeconds
    , _uri :: !Text
    , _tags :: ![Text]
    } deriving (Show, Generic)

makeLenses ''BookmarkCloud
makeLenses ''BookmarkJSON

insertBookmarks :: [BookmarkJSON] -> BookmarkCloud -> BookmarkCloud
insertBookmarks = execState . updateCloud
  where
    updateCloud bs = do
      xs <- use bookmarkCloudBookmarks

      let combineBookmarks = forM_ bs $ \b -> do
            x <- use $ at b . non b

            let da = min (x ^. dateAdded) (b ^. dateAdded)
                dm = max (x ^. lastModified) (b ^. lastModified)
                ts = nub (x ^. tags ++ b ^. tags)
                x' = x & title .~ b ^. title
                       & dateAdded .~ da
                       & lastModified .~ dm
                       & tags .~ ts
        
            at b ?= x'

      let bm = M.fromList $ zip xs xs
          bs' = M.elems $ execState combineBookmarks bm
          
      bookmarkCloudBookmarks .= bs'
      bookmarkCloudLastModified .= currentTimeInPOSIXMicroSeconds

makeBookmark :: (Bookmarkable b) => b -> BookmarkJSON
makeBookmark b =
  let t  = bookmarkTitle b
      da = bookmarkDateAdded b
      dm = bookmarkLastModified b
      u  = bookmarkUri b
      ts = bookmarkTags b
  in BookmarkJSON t da dm u ts

defaultBookmarkCloud :: BookmarkCloud
defaultBookmarkCloud = BookmarkCloud d d []
  where d = currentTimeInPOSIXMicroSeconds

instance Bookmarkable BookmarkJSON where
  bookmarkTitle        = view title
  bookmarkDateAdded    = view dateAdded
  bookmarkLastModified = view lastModified
  bookmarkUri          = view uri
  bookmarkTags         = view tags
  
instance ToJSON BookmarkJSON
instance FromJSON BookmarkJSON

instance FromJSON BookmarkCloud
instance ToJSON BookmarkCloud

instance Ord BookmarkJSON where
  compare = compare `on` _uri

instance Eq BookmarkJSON where
  (==) = (==) `on` _uri

instance Eq BookmarkCloud where
  (==) = (==) `on` _bookmarkCloudBookmarks
