----------------------------------------------------------------------
-- |
-- Module : Cloud.Core.Tagable
--
----------------------------------------------------------------------


module Cloud.Core.Taggable
    (
      Taggable (..)
    ) where


import           Data.Text   (Text)
import           Cloud.Utils


class Taggable t where
    tagTitle         :: t -> Text
    tagDateAdded     :: t -> POSIXMicroSeconds
    tagLastModified  :: t -> POSIXMicroSeconds
    tagBookmarkLinks :: t -> [Text]
    tagHash          :: t -> Int
    tagHash          = textHash . tagTitle

