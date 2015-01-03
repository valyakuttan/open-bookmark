{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : BookmarkCloud.Core.Bookmarkable
--
----------------------------------------------------------------------


module BookmarkCloud.Core.Bookmarkable
    (
      Bookmarkable (..)
    ) where


import           Data.Maybe
import           Data.Text           (Text)

import           BookmarkCloud.Utils


class Bookmarkable b where
  bookmarkTitle        :: b -> Maybe Text
  bookmarkDateAdded    :: b -> Maybe POSIXMicroSeconds
  bookmarkLastModified :: b -> Maybe POSIXMicroSeconds
  bookmarkUri          :: b -> Maybe Text
  bookmarkTags         :: b -> [Text]
  bookmarkHash         :: b -> Int
  bookmarkHash         = textHash . fromMaybe "". bookmarkUri
