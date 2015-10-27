{-# LANGUAGE OverloadedStrings #-}

module Cloud.EngineSpec where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Text           (pack)
import           Test.Hspec

import           Cloud.App
import           Cloud.Config
import           Cloud.Engine
import           Cloud.Types


type BookmarkCloud = HCloud Bookmark


applyOnListSpec :: Spec
applyOnListSpec = do
    describe "applyOnList" $ do
        it "should work with insertWith" $ do
            let bs = bookmarks
                ins = insertWith const
            Right cs <- appRun $ applyOnList readCloudWithDefault ins bs
            forM_ bs $ \b -> do
                search' b cs `shouldBe` Just b

        it "should work with insertWith" $ do
            let bs = bookmarks
                ins = insertWith const
                del = update $ const Nothing
            Right cs <- appRun $ applyOnList readCloudWithDefault ins bs
            forM_ bs $ \b -> do
                search' b cs `shouldBe` Just b

            Right cs' <- appRun $ applyOnList readCloudWithDefault del bs
            forM_ bs $ \b -> do
                search' b cs' `shouldBe` Nothing

updateSpec :: Spec
updateSpec = do
    describe "update" $ do
        it "when f return Nothing should remove element" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            let h = insertWith const b e
            Right c <- appRun h
            search b c `shouldBe` Just b

            let g = update (const Nothing) b c
            Right c' <- appRun g
            search b c' `shouldBe` Nothing

        it "when f return Just y should modify element" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            let h = insertWith const b e
            Right c <- appRun h
            search b c `shouldBe` Just b

            let b' = b & bookmarkTitle .~ "test"
                f _ = Just b'
            let g = update f b c
            Right c' <- appRun g
            let (Just x) = search b c'
            (x ^. bookmarkTitle) `shouldBe` "test"

insertWithSpec :: Spec
insertWithSpec = do
    describe "insertWith" $ do
        it "should add element to the cloud" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            let h = insertWith const b e
            Right c <- appRun h
            search b c `shouldBe` Just b

        it "should modify an existing element" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            let h = insertWith const b e
            Right c <- appRun h
            search b c `shouldBe` Just b

            let b' = b & bookmarkTitle .~ "test"
                f _ _ = b'
            let g = insertWith f b c
            Right c' <- appRun g
            let (Just x) = search b c'
            (x ^. bookmarkTitle) `shouldBe` "test"

searchSpec :: Spec
searchSpec = do
    describe "search" $ do
        it "should return Nothing when not found" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            search b e `shouldBe` Nothing

        it "shoud return Just b when found" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            let h = insertWith const b e
            Right c <- appRun h
            search b c `shouldBe` Just b

isEmptySpec :: Spec
isEmptySpec = do
    describe "isEmpty" $ do
        it "should return True for an empty cloud" $ do
            let b = emptyBookmark
            e <- emptyCloud b
            e `shouldSatisfy` isEmpty

        it "should return False for a nonempty cloud" $ do
            let b = emptyBookmark
            e       <- emptyCloud b
            let c = insertWith const b e
            Right c' <- appRun c
            c' `shouldSatisfy` not . isEmpty

appRun :: App a -> IO (Either String a)
appRun a = defaultAppEnvironment "/tmp" "tmp.cfg" >>= runApp a

emptyCloud :: Bookmark -> IO BookmarkCloud
emptyCloud b = do
    c <- appRun $ readCloudWithDefault b
    return $ either (const undefined) id c

bookmarks :: [Bookmark]
bookmarks = map mkB [0..n]
  where
      n = 3 * maxNumberOfClouds defaultConfig
      mkB i = emptyBookmark & bookmarkUrl .~ pack ("u" ++ show i)

search' :: Bookmark -> [BookmarkCloud] -> Maybe Bookmark
search' b cs = foldr (<|>) Nothing $ search b <$> cs

spec :: Spec
spec = do
    isEmptySpec
    searchSpec
    insertWithSpec
    updateSpec
    applyOnListSpec
