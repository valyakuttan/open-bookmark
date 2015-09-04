module Cloud.CloudSpec where


import           Test.Hspec

import           Cloud.Cloud
import           Cloud.Types
import           Cloud.Utils


type BC = Cloud Bookmark


emptyCloudSpec :: Spec
emptyCloudSpec = do
    describe "emptyCloud" $ do
        it "should create an empty store" $ do
            let i = integerToPOSIXMicroSeconds 0
            let c = emptyCloud i i :: BC
            isEmpty c `shouldBe` True

        it "should return Nothing for any lookup" $ do
            let i = integerToPOSIXMicroSeconds 0
            let c = emptyCloud i i :: BC
            search emptyBookmark c `shouldBe` Nothing

insertWithSpec :: Spec
insertWithSpec = do
    describe "InsertWith" $ do
        it "should return what is inserted" $ do
            let i = integerToPOSIXMicroSeconds 0
            let c = emptyCloud i i :: BC
            let b = emptyBookmark
            let c1 = insertWith i const b c

            search b c1 `shouldBe` Just b

updateSpec :: Spec
updateSpec = do
    describe "update" $ do
        it "should delete what is in the store" $ do
            let i = integerToPOSIXMicroSeconds 0
            let c = emptyCloud i i :: BC
            let b = emptyBookmark
            let c1 = insertWith i const b c

            search b c1 `shouldBe` Just b

            let c2 = update i (const Nothing) b c1

            search b c2 `shouldBe` Nothing

spec :: Spec
spec = do
    emptyCloudSpec
    insertWithSpec
    updateSpec
