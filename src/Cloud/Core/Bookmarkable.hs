{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Cloud.Core.Bookmarkable
--
----------------------------------------------------------------------


module Cloud.Core.Bookmarkable
    (
      Bookmarkable (..)
    ) where


import           Data.Text
import           Cloud.Utils


class Bookmarkable b where
    bookmarkTitle        :: b -> Text
    bookmarkDateAdded    :: b -> POSIXMicroSeconds
    bookmarkLastModified :: b -> POSIXMicroSeconds
    bookmarkUri          :: b -> Text
    bookmarkTags         :: b -> [Text]
    bookmarkHash         :: b -> Int
    bookmarkHash         = textHash . bookmarkUri
