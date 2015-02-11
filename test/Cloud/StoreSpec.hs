{-# LANGUAGE OverloadedStrings #-}

module Cloud.StoreSpec where


import           Control.Lens
import           Test.Hspec

import           Cloud.Store
import           Cloud.Types


emptyStoreSpec :: Spec
emptyStoreSpec = do
    describe "emptyStore" $ do
        it "should create an empty store" $ do
            let s = emptyStore :: Store Bookmark
            isEmpty s `shouldBe` True

        it "should return Nothing for any lookup" $ do
            let lookup' = Cloud.Store.lookup
            lookup' emptyBookmark emptyStore `shouldBe` Nothing

singletonSpec :: Spec
singletonSpec = do
    describe "singleton" $ do
        it "should create a nonempty store" $ do
            let s = singleton emptyBookmark
            isEmpty s `shouldBe` False

        it "should contain the element" $ do
            let lookup' = Cloud.Store.lookup
                s       = singleton emptyBookmark
            lookup' emptyBookmark s `shouldBe` Just emptyBookmark
insertWithSpec :: Spec
insertWithSpec = do
    describe "InsertWith" $ do
        it "should return a singleton" $ do
            let insert' = insertWith const emptyBookmark
            insert' emptyStore `shouldBe` singleton emptyBookmark

        it "should return what is inserted" $ do
            let b = emptyBookmark & bookmarkTitle .~ "test"
                s = insertWith const b emptyStore
            Cloud.Store.lookup b s `shouldBe` Just b

        it "should update element in the store" $ do
            let b       = emptyBookmark & bookmarkTitle .~ "test"
                s       = insertWith const b emptyStore
                b1      = b & bookmarkTitle .~ "update"
                s1      = insertWith (\ _ _ -> b1) b s
                Just b2 = Cloud.Store.lookup b s1

            b2 ^. bookmarkTitle `shouldBe` "update"

updateSpec :: Spec
updateSpec = do
    describe "update" $ do
        it "should delete what is in the store" $ do
            let b = emptyBookmark & bookmarkTitle .~ "test"
                s = insertWith const b emptyStore

            Cloud.Store.lookup b s `shouldBe` Just b

            let s1 = update (const Nothing) b s

            Cloud.Store.lookup b s1 `shouldBe` Nothing

        it "should update element in the store" $ do
            let b       = emptyBookmark & bookmarkTitle .~ "test"
                s       = insertWith const b emptyStore
                b1      = b & bookmarkTitle .~ "update"
                s1      = update (const $ Just b1) b s
                Just b2 = Cloud.Store.lookup b s1

            b2 ^. bookmarkTitle `shouldBe` "update"

spec :: Spec
spec = do
    emptyStoreSpec
    singletonSpec
    insertWithSpec
    updateSpec
