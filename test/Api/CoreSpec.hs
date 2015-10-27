{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Api.CoreSpec where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Maybe
import           Data.Text              (pack, unpack)
import           Test.Hspec

import           Api.Core
import           Cloud.App
import           Cloud.Engine
import           Cloud.Types


type BookmarkCloud = HCloud Bookmark
type TagCloud = HCloud Tag

removeBookmarkSpec :: Spec
removeBookmarkSpec = describe "removeBookmark" $ do
    it "should return Nothing if bookmark is abscent" $ do
        let f = readCloudWithDefault
            g = readCloudWithDefault

        r <- unwrapApp $ removeBookmark f g "u1"
        r `shouldBe` Nothing

    it "should clean bookmark and tag entries while removing" $ do
        let url   = "u1"
            tags  = ["t1"]

        (bc1,[tc1]) <- newBcWithTcs url tags

        let f1 _ = return bc1
            g1 _ = return tc1
        (bc2, [tc2]) <- unwrapAppMaybe $ removeBookmark f1 g1 "u1"

        let f2 _ = return bc2
            g2 _ = return tc2
        r1 <- unwrapApp $ searchBookmark f2 "u1"
        r2 <- unwrapApp $ searchTag g2 "t1"

        r1 `shouldBe` Nothing
        r2 `shouldBe` Nothing

    it "should clean orphan tags" $ do
        let url   = "u1"
            tags  = ["t1"]

        (bc1,[tc1]) <- newBcWithTcs url tags

        let f1 _ = return bc1
            g1 _ = return tc1
        (bc2, [tc2]) <- unwrapAppMaybe $ removeBookmark f1 g1 "u1"

        bc2 `shouldSatisfy` isEmpty
        tc2 `shouldSatisfy` isEmpty

removeTagSpec :: Spec
removeTagSpec = describe "removeTag" $ do
    it "should return Nothing if bookmark is abscent" $ do
        let f = readCloudWithDefault
            g = readCloudWithDefault

        r <- unwrapApp $ removeTag f g "t1" "u1"
        r `shouldBe` Nothing

    it "should not affect bookmark cloud if tag is abscent" $ do
        let url   = "u1"
            tags  = ["t1", "t2"]

        (bc,_) <- newBcWithTcs url tags

        let f1 _ = return bc
            g1 = readCloudWithDefault
        (bc1,_) <- unwrapAppMaybe $ removeTag f1 g1 "test" url
        testBookmarkCloud "title" url tags bc1

    it "should clean bookmark and tag entries while removing" $ do
        let url   = "u1"
            tags  = ["t1", "t2"]

        (bc1,tcs1) <- newBcWithTcs url tags

        let f1 _ = return bc1
            h    = return . fromJust . getTC tcs1
            g1 t = h $ unpack (t ^. tagTitle)
            add2 = addBookmark
                   readCloudWithDefault
                   g1
        (_, tcs2) <- unwrapApp $ add2 "test" "u2" ["t1"]

        let tcs = tcs2 ++ tcs1
            g2 t = return $ fromJust $ getTC tcs $ unpack (t ^. tagTitle)

        (bc,tc) <- unwrapAppMaybe $ removeTag f1 g2 "t1" url

        testBookmarkCloud "title" url ["t2"] bc
        t <- unwrapAppMaybe $ searchTag (const $ return tc) "t1"

        t ^. bookmarkUrls `shouldBe` ["u2"]

    it "should clean orphan tags" $ do
        let url   = "u1"
            tags  = ["t1"]
        (bc1,[tc1]) <- newBcWithTcs url tags

        let f1 _ = return bc1
            g1 _ = return tc1

        (bc,tc) <- unwrapAppMaybe $ removeTag f1 g1 "t1" url

        testBookmarkCloud "title" url [] bc
        tc `shouldSatisfy` isEmpty

attachTagSpec :: Spec
attachTagSpec = describe "attachTag" $ do
    it "should should not attach a tag if bookmark is abscent" $ do
        let f = readCloudWithDefault
            g = readCloudWithDefault

        r <- unwrapApp $ attachTag f g "t1" "u1"
        r `shouldBe` Nothing

    it "should add a new tag if bookmark is present" $ do
        let title = "title"
            url   = "u1"
            tags  = ["t1", "t2"]

        (bc,tcs) <- newBcWithTcs url tags
        testBookmarkCloud title url tags bc
        testTagClouds url tags tcs

        let f1 _ = return bc
            g1 t = readCloudWithDefault t
        (bc1,tc1) <- unwrapAppMaybe $ attachTag f1 g1 "test" url

        testBookmarkCloud title url (tags ++ ["test"]) bc1
        testTagClouds url ["test"] [tc1]

    it "should attach an existing tag to a bookmark, if present" $ do
        let title1 = "title"
            url1   = "u1"
            tags1  = ["t1", "t2"]

        (bc1,tcs) <- newBcWithTcs url1 tags1
        testBookmarkCloud title1 url1 tags1 bc1
        testTagClouds url1 tags1 tcs

        let url2 = "u2"
            tag2 = "t1"
        (bc2,_) <- newBcWithTcs url2 []
        let f2 _ = return bc2
            g2 t = return $ fromJust $ getTC tcs $ unpack (t ^. tagTitle)
        (bc, tc) <- unwrapAppMaybe $ attachTag f2 g2 tag2 url2
        testBookmarkCloud title1 url2 [tag2] bc
        testTagClouds url2 [tag2] [tc]

addBookmarkSpec :: Spec
addBookmarkSpec = describe "addBookmark" $ do
    it "should add a new bookmark along with its tags" $ do
        let title = "title"
            url   = "u1"
            tags  = ["t1", "t2"]
        (bc,tcs) <- newBcWithTcs url tags

        testBookmarkCloud title url tags bc
        testTagClouds url tags tcs

    it "should update tags of an existing bookmark" $ do
        let title = "title"
            url   = "u1"
            tags  = ["t1", "t2"]
        (bc,tcs) <- newBcWithTcs url tags

        let f _ = return bc

            g t | unpack (t ^. tagTitle)  `elem` tags = do
                let (Just c) = getTC tcs $ unpack (t ^. tagTitle)
                return c
                | otherwise = readCloudWithDefault t

            add2 = addBookmark f g
            tags2 = tags ++ ["t3"]

        (bc2, tcs2) <- unwrapApp $ add2 title url tags2

        testBookmarkCloud title url tags2 bc2
        testTagClouds url tags2 tcs2

    it "should update urls of an existing tag" $ do
        let title = "title"
            url   = "u1"
            tags  = ["t1", "t2"]
        (bc,tcs) <- newBcWithTcs url tags

        let f _ = return bc

            g t | unpack (t ^. tagTitle)  `elem` tags = do
                let (Just c) = getTC tcs $ unpack (t ^. tagTitle)
                return c
                | otherwise = readCloudWithDefault t

            add2 = addBookmark f g
            tags2 = tags ++ ["t3"]
        (bc2, tcs2) <- unwrapApp $ add2 title url tags2

        testBookmarkCloud title url tags2 bc2
        testTagClouds url tags2 tcs2

        let f3 b | unpack (b ^. bookmarkUrl) == "u1" = return bc
                | otherwise = readCloudWithDefault b

            g3 t | unpack (t ^. tagTitle)  `elem` tags2 = do
                let (Just c) = getTC tcs2 $ unpack (t ^. tagTitle)
                return c
                | otherwise = readCloudWithDefault t

            add3 = addBookmark f3 g3
            url3 = "u3"
            title3 = "title3"
            tags3 = ["t1", "t3", "t4"]
        (bc3, tcs3) <- unwrapApp $ add3 title3 url3 tags3

        testBookmarkCloud title3 url3 tags3 bc3
        testTagClouds url3 tags3 tcs3
        testTagClouds url ["t1", "t3"] tcs3

searchBookmarkSpec :: Spec
searchBookmarkSpec = describe "searchBookmark" $ do
    it "should return Nothing when not found" $ do
        r <- appRun $ searchBookmark readCloudWithDefault "url"
        r `shouldBe` Right Nothing

    it "should return Just v when found" $ do
        let b = emptyBookmark & bookmarkUrl .~  "url"
        Right c <- appRun $ readCloudWithDefault b
        Right c1 <- appRun $ insertWith const b c
        let f _ = return c1
        Right r <- appRun $ searchBookmark f "url"
        r `shouldBe` Just b

searchTagSpec :: Spec
searchTagSpec = describe "searchTag" $ do
    it "should return Nothing when not found" $ do
        r <- appRun $ searchTag readCloudWithDefault "tag"
        r `shouldBe` Right Nothing

    it "should return Just v when found" $ do
        let t = emptyTag & tagTitle .~  "tag"
        Right c <- appRun $ readCloudWithDefault t
        Right c1 <- appRun $ insertWith const t c
        let f _ = return c1
        Right r <- appRun $ searchTag f "tag"
        r `shouldBe` Just t

spec :: Spec
spec = do
    searchBookmarkSpec
    searchTagSpec
    addBookmarkSpec
    attachTagSpec
    removeTagSpec
    removeBookmarkSpec

testTagClouds :: String -> [String] -> [TagCloud] -> IO ()
testTagClouds url tags tcs = forM_ tags $ \t -> do
    let r = getTC tcs t
    r `shouldSatisfy` isJust

    let Just c = r
        f _ = return c

    x <- appRun $ searchTag f t
    x `shouldSatisfy` isRight
    let Right y = x
    y `shouldSatisfy` isJust
    let Just t' = y

    (t' ^. tagTitle) `shouldBe` pack t
    (t' ^. bookmarkUrls) `shouldSatisfy` elem (pack url)

testBookmarkCloud :: String
                   -> String
                   -> [String]
                   -> BookmarkCloud
                   -> IO ()
testBookmarkCloud title url tags bc = do
    let fb _ = return bc
    x <- appRun $ searchBookmark fb url
    x `shouldSatisfy` isRight
    let Right y = x
    y `shouldSatisfy` isJust
    let Just b' = y
    (b' ^. bookmarkUrl) `shouldBe` pack url
    (b' ^. bookmarkTitle) `shouldBe` pack title
    (b' ^. bookmarkTags) `shouldMatchList` map pack tags

newBcWithTcs :: String -> [String] -> IO (BookmarkCloud, [TagCloud])
newBcWithTcs url tags = do
    let title = "title"
        add1 = addBookmark
               readCloudWithDefault
               readCloudWithDefault
    unwrapApp $ add1 title url tags

unwrapApp :: MonadIO m => App a -> m a
unwrapApp a = liftIO $ either error id <$> appRun a

unwrapAppMaybe :: MonadIO m => App (Maybe a) -> m a
unwrapAppMaybe a = unwrapApp a >>= return . fromJust

appRun :: App a -> IO (Either String a)
appRun a = defaultAppEnvironment "/tmp" "tmp.cfg" >>= runApp a

getTC :: [TagCloud] -> String -> Maybe TagCloud
getTC tcs title = foldr ((<|>) . f) Nothing tcs
  where
      f c = const c <$> search t c
      t = emptyTag & tagTitle .~ pack title

getBC :: [BookmarkCloud] -> String -> Maybe BookmarkCloud
getBC bcs url = foldr ((<|>) . f) Nothing bcs
  where
      f c = const c <$> search t c
      t = emptyBookmark & bookmarkUrl .~ pack url
