{-# OPTIONS_GHC -fno-warn-orphans #-}


module Cloud.TypesSpec where


import Data.Maybe
import Control.Applicative
import Control.Lens
import Data.Text (pack)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

import Cloud.Types
import Cloud.Utils


instance Arbitrary POSIXMicroSeconds where
    arbitrary = integerToPOSIXMicroSeconds <$> arbitrary

instance Arbitrary Bookmark where
    arbitrary = mkBookmark <$> arbitrary <*>
                arbitrary  <*> arbitrary <*>
                arbitrary  <*> arbitrary

instance Arbitrary Tag where
    arbitrary = mkTag <$> arbitrary <*>
                arbitrary  <*> arbitrary <*>
                arbitrary

spec :: Spec
spec = do
    describe "JSON conversion" $ do
        prop "convert to and from bookmark" $ \b ->
            fromJSONData (toJSONData b) ==* (b :: Bookmark)

        prop "convert to and from tag" $ \t ->
            fromJSONData (toJSONData t) ==# (t :: Tag)

toJSONData :: ToJSON a => a -> ByteString
toJSONData = encode

fromJSONData :: FromJSON a => ByteString -> a
fromJSONData = fromJust . decode

mkBookmark :: String
           -> POSIXMicroSeconds
           -> POSIXMicroSeconds
           -> String
           -> [String]
           -> Bookmark
mkBookmark t a m u ts = emptyBookmark &
                        bookmarkTitle.~ pack t &
                        bookmarkDateAdded .~ a &
                        bookmarkLastModified .~ m &
                        bookmarkUrl.~ pack u &
                        bookmarkTags.~ map pack ts

mkTag :: String
      -> POSIXMicroSeconds
      -> POSIXMicroSeconds
      -> [String]
      -> Tag
mkTag t a m us = emptyTag &
                 tagTitle .~ pack t &
                 tagDateAdded .~ a &
                 tagLastModified .~ m &
                 bookmarkUrls .~ map pack us
                 
(==*) :: Bookmark -> Bookmark -> Bool
a ==* b = a ^. bookmarkTitle == b ^. bookmarkTitle &&
          a ^. bookmarkDateAdded == b ^. bookmarkDateAdded &&
          a ^. bookmarkLastModified == b ^. bookmarkLastModified &&
          a ^. bookmarkUrl == b ^. bookmarkUrl &&
          a ^. bookmarkTags == b ^. bookmarkTags

(==#) :: Tag -> Tag -> Bool
a ==# b = a ^. tagTitle == b ^. tagTitle &&
          a ^. tagDateAdded == b ^. tagDateAdded &&
          a ^. tagLastModified == b ^. tagLastModified &&
          a ^. bookmarkUrls == b ^. bookmarkUrls
