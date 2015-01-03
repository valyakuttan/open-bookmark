{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Api.Core
--
----------------------------------------------------------------------


module Api.Core
    (
      -- * Basic Types to work with clouds
      Bookmarkable (..)
    , BookmarkJSON
    , BookmarkCloud
    , Taggable (..)
    , TagJSON
    , TagCloud
    
      -- * App utilities
    , App
    , runApp
    
      -- * File System manipulation
    , bookmarkCloudDirectoryPath
    , tagCloudDirectoryPath
    , bookmarkCloudFilePath
    , tagCloudFilePath

      -- * Query bookmark
    , searchBookmark

      -- * Query tag
    , searchTag
    
      -- * BookmarkCloud manipulation
    , addBookmark
    , addBookmarkWith
    , readBookmarkCloud
    , writeBookmarkCloud
    ) where


import           App
import           BookmarkCloud.Config
import           BookmarkCloud.Core.Bookmark
import           BookmarkCloud.Core.Tag


type Tag = String
type Url = String

searchBookmark :: Url -> App (Maybe BookmarkJSON)
searchBookmark = undefined

addBookmark :: Bookmarkable b => b
            -> App BookmarkCloud
            -> App BookmarkCloud
addBookmark = undefined

addBookmarkWith :: Bookmarkable b => (b -> b -> b)
                -> b
                -> App BookmarkCloud
                -> App BookmarkCloud
addBookmarkWith = undefined

writeBookmarkCloud :: App BookmarkCloud -> App ()
writeBookmarkCloud = undefined

readBookmarkCloud :: Bookmarkable b => b -> App BookmarkCloud
readBookmarkCloud = undefined

searchTag :: Tag -> App (Maybe TagJSON)
searchTag = undefined
