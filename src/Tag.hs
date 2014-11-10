{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

module Tag (
      Tagable (..)
    , TagCloud
    , TagJSON
    , defaultTagCloud
    , insertTags
    , makeTag
    , tagCloudCreated
    ) where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.State (execState)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function (on)
import qualified Data.Map as M
import Data.List (nub)
import Data.Text (Text)
import GHC.Generics

import Utils

class Tagable t where
  tagTitle         :: t -> Text
  tagDateAdded     :: t -> POSIXMicroSeconds
  tagLastModified  :: t -> POSIXMicroSeconds
  tagBookmarkLinks :: t -> [Text]
  tagHash          :: t -> Int
  tagHash          = textHash . tagTitle

data TagCloud = TagCloud {
      tagCloudCreated :: !POSIXMicroSeconds
    , _tagCloudLastModified :: !POSIXMicroSeconds
    , _tagCloudTags :: ![TagJSON]
    } deriving (Show, Generic)

data TagJSON = TagJSON {
      _title          :: !Text
    , _dateAdded      :: !POSIXMicroSeconds
    , _lastModified   :: !POSIXMicroSeconds
    , _bookmarkLinks  :: ![Text]
    } deriving (Show, Generic)

makeLenses ''TagCloud
makeLenses ''TagJSON

insertTags :: [TagJSON] -> TagCloud -> TagCloud
insertTags = execState . updateCloud
  where
    updateCloud ts = do
      xs <- use tagCloudTags

      let combineTags = forM_ ts $ \t -> do
            x <- use $ at t . non t
            
            let ls = nub ( x ^. bookmarkLinks ++ t ^. bookmarkLinks)
                da = min (x ^. dateAdded) (t ^. dateAdded)
                dm = max (x ^. lastModified) (t ^. lastModified)
                x' = x & dateAdded .~ da
                       & lastModified .~ dm
                       & bookmarkLinks .~ ls

            at t ?= x'
            
      let tm  = M.fromList $ zip xs xs
          ts' = M.elems $ execState combineTags tm 
              
      tagCloudTags .= ts'
      tagCloudLastModified .= currentTimeInPOSIXMicroSeconds

makeTag :: Text -> POSIXMicroSeconds -> POSIXMicroSeconds -> [Text] -> TagJSON
makeTag = TagJSON

defaultTagCloud :: TagCloud
defaultTagCloud = TagCloud d d []
  where d = currentTimeInPOSIXMicroSeconds

instance Tagable TagJSON where
  tagTitle = view title
  tagDateAdded = view dateAdded
  tagLastModified = view lastModified
  tagBookmarkLinks = view bookmarkLinks

instance Ord TagJSON where
  compare = compare `on` _title

instance Eq TagJSON where
  (==) = (==) `on` _title

instance Eq TagCloud where
  (==) = (==) `on` _tagCloudTags

instance FromJSON TagCloud
instance ToJSON TagCloud
instance FromJSON TagJSON
instance ToJSON TagJSON
