{-# LANGUAGE TupleSections #-}


module Api.Bookmark
     (
       -- * Query bookmark cloud
       cloudDirectory
     , searchBookmark
     , searchTag

       -- * Modify bookmark cloud
     , importBookmarks
     , addBookmark
     , attachTag
     , removeBookmark
     , removeTag
     )  where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Function       (on)
import           Data.List           (groupBy, sortBy, (\\))
import           Data.Text           (Text, pack)

import           Api.Cloud
import           App
import           Cloud.Engine


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text (Text, append, pack)
-- >>> import           Cloud.Utils (integerToPOSIXMicroSeconds)
-- >>> import Control.Applicative
-- >>> import qualified Cloud.Config as Cfg
-- >>> import qualified Cloud.Core.Cloud as Cloud
--
-- >>> let cfg = Cfg.defaultConfig
-- >>> let root = "/tmp"
-- >>> let i2p = integerToPOSIXMicroSeconds
-- >>> e <- getDefaultAppEnvironment root
-- >>> let appRun = flip runApp e
-- >>> let insert = insertWith const
-- >>> let delete = update (const Nothing)
-- >>> let maybeWith f b = maybe False (f(bookmarkableToBookmark b))
-- >>> let memberWith' f b bc = appRun (maybeWith f b <$> search b bc)
-- >>> let g = either (const False) id
-- >>> let memberWith f b = fmap g . memberWith' f b
-- >>> let member = memberWith (==)
--
-- >>> data BM = BM { tu :: !Text, ttgs :: ![Text] } deriving (Show)
--
-- >>> instance Bookmarkable BM where { bookmarkTitle _ = ""; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tu; bookmarkTags = ttgs; bookmarkType _ = Book }
--
-- >>> instance Eq BM where { (==) = (==) `on` tu }
--
-- >>> data BT = BT { tgt :: !Text, tgtgs :: ![Text] } deriving (Show)
--
-- >>> instance Eq BT where { (==) = (==) `on` tgt }
--
-- >>> instance Bookmarkable BT where { bookmarkTitle = tgt; bookmarkDateAdded _ = i2p 0; bookmarkLastModified _ = i2p 10; bookmarkUri = tgt; bookmarkTags = tgtgs; bookmarkType _ = Tag }
--
-- >>> let i2t p i = append p $ pack $ show i
-- >>> let fpath = Cfg.cloudFilePath cfg root
--
-- >>> let mksampleBM i = BM (i2t "u" i) (map (i2t "t") [1..i])
-- >>> let mksamplesBM i = map mksampleBM [0..i]
-- >>> let mksampleBT i = BT (i2t "u" i) (map (i2t "t") [1..i])
-- >>> let mksamplesBT i = map mksampleBT [0..i]
-- >>> let sampleSize = 2 * maximumNumberOfClouds Cfg.defaultConfig
-- >>> let sampleBMs = mksamplesBM sampleSize
-- >>> let sampleBTs = mksamplesBT sampleSize
--
-- >>> let updateBookmark' = updateBookmarkWith' (const False)
-- >>> let removeBookmark' = updateBookmark' $ const Nothing


type Url = Text
type Tag = Text

-- | Search a url in the bookmark cloud. If found
-- return @'App'('Just' bookmark)@, otherwise
-- return 'App Nothing'.
searchBookmark :: String -> App (Maybe Bookmark)
searchBookmark url = readCloudWithDefault b >>= search b
  where b = emptyBookmark & uri .~ pack url

-- | Search a tag name in the bookmark cloud. If found
-- return @App('Just' tag)@, otherwise return 'App Nothing'.
searchTag :: String -> App (Maybe BookmarkTag)
searchTag t = readCloudWithDefault tg >>= fmap f . search tg
  where
      tg = emptyBookmarkTag & tagTitle .~ pack t
      f  = fmap bookmarkToBookmarkTag

-- | Bookmark a url and attch title and tags to it.
addBookmark :: String -> String -> [String] -> App ()
addBookmark btitle url tgs = createBookmark btitle url tgs >> mapM_ f tgs
  where f = flip attachTag url

-- | Create a bookmark entry in the cloud with given attributes.
createBookmark :: String -> String -> [String] -> App ()
createBookmark btitle url tgs = do
    b   <- mkBookmark btitle url tgs
    bc  <- readCloudWithDefault b
    bc' <- addBookmark' b bc
    writeCloud bc'

-- | Create a tag entry in the cloud with given attributes.
createTag :: String -> String -> App ()
createTag tag url = do
    t   <- mkTag tag url
    tc  <- readCloudWithDefault t
    tc' <- addBookmark' t tc
    writeCloud tc'

-- | Attach a tag to the given url.
attachTag :: String -> String -> App ()
attachTag tag url = do
    x  <- mkBookmark "" url [tag]
    bc <- readCloudWithDefault x
    r  <- search x bc
    case r of
        Just _  -> do
            bc' <- addBookmark' x bc
            writeCloud bc'
            createTag tag url
        Nothing -> return ()

-- | Remove the bookmark enty associated with @url@ from the cloud.
-- Also it will update tag entries which are attached to this
-- @url@.
removeBookmark :: String -> App ()
removeBookmark url = do
    x  <- mkBookmark "" url []
    bc <- readCloudWithDefault x
    r  <- search x bc
    case r of
        Nothing -> return ()
        Just b  -> do
            tss <- groupBookmarks $ getTags b
            tcs' <- forM tss $ \ts -> do
                tc <- readCloudWithDefault $ head ts
                foldM removeUrl' tc ts

            bc' <- removeBookmark' b bc
            mapM_ writeIfNonempty $ bc' : tcs'
  where
      removeBookmark' = updateBookmarkWith' (const False) (const Nothing)
      removeUrl'      = flip $ removeUrlFromTag' $ pack url

-- | Remove tag attached to the url by removing it from from the
-- bookmark entry. Also it will remove url from the links of tag entry.
removeTag :: String -> String -> App ()
removeTag tag url = do
    x  <- mkTag tag ""
    tc <- readCloudWithDefault x
    r  <- search x tc
    case r of
        Nothing -> return ()
        Just t  -> do
            y <- mkBookmark "" url []
            bc <- readCloudWithDefault y
            s  <- search y bc
            case s of
                Nothing -> return ()
                Just b  -> do
                    bc' <- removeTagFromBookmark' (t ^. title) b bc
                    writeIfNonempty bc'

            tc' <- removeUrl(pack url) t tc
            writeIfNonempty tc'
  where
      removeUrl u t = removeUrlFromTag' u $ bookmarkToBookmarkTag t

importBookmarks :: Bookmarkable b => [b] -> App ()
importBookmarks = addBookmarks

writeIfNonempty :: BookmarkCloud -> App ()
writeIfNonempty c | isEmpty c = removeCloud c
                  | otherwise = writeCloud c

-- | Remove url from a tag's links. If links are
-- empty the bookmark tag is removed from bookmark cloud.
--
-- >>> let xs = sampleBMs
-- >>> Right bss <- appRun (groupBookmarks xs)
-- >>> let bs = head bss
-- >>> let b = bookmarkableToBookmark (bs !! 1)
-- >>> let t = getTags b !! 1
-- >>> Right tc <- appRun (emptyBookmarkCloud t)
-- >>> Right tc' <- appRun (addBookmark' t tc)
--
-- check insertion
--
-- >>> after <- member t tc'
-- >>> after
-- True
--
-- check removal of url
--
-- >>> let f b = b & tags %~ (++ ["test-tag"])
-- >>> let x = updateBookmarkWith' (const False) (Just . f) t tc'
-- >>> Right tc1 <- appRun x
-- >>> let u = bookmarkUri b
-- >>> Right (Just t') <- appRun (search t tc1)
-- >>> u `elem` (t' ^. tags)
-- True
-- >>> Right tc2 <- appRun (removeUrlFromTag' u t tc1)
-- >>> Right (Just t') <- appRun (search t tc2)
-- >>> u `elem` (t' ^. tags)
-- False
-- >>> Right tc3 <- appRun (removeUrlFromTag' "test-tag" t tc2)
-- >>> Right tag <- appRun (search t tc3)
-- >>> tag
-- Nothing
removeUrlFromTag' :: Url
                  -> BookmarkTag
                  -> BookmarkCloud
                  -> App BookmarkCloud
removeUrlFromTag' url = updateBookmarkWith' check (Just . update')
  where
      check b = null $ b ^. tags
      update' b = b & tags %~ filter (/= url)

-- | Remove tag from bookmark's tag list.
--
-- >>> let xs = sampleBMs
-- >>> Right bss <- appRun (groupBookmarks xs)
-- >>> let bs = head bss
-- >>> let b = bookmarkableToBookmark (bs !! 1)
-- >>> Right bc <- appRun (emptyBookmarkCloud b)
-- >>> Right bc' <- appRun (addBookmark' b bc)
--
-- check insertion
--
-- >>> after <- member b bc'
-- >>> after
-- True
--
-- check removal of tag
--
-- >>> let t = (getTags b !! 1) ^. tagTitle
-- >>> Right bc1 <- appRun (removeTagFromBookmark' t b bc')
-- >>> Right (Just b') <- appRun (search b bc1)
-- >>> t `elem` (b' ^. tags)
-- False
removeTagFromBookmark' :: Tag
                       -> Bookmark
                       -> BookmarkCloud
                       -> App BookmarkCloud
removeTagFromBookmark' tg = updateBookmarkWith' (const False) (Just . f)
  where f x = x & tags %~ filter (/= tg)

-- | Add a list of bookmarkable to bookmark cloud.
--
addBookmarks :: Bookmarkable b => [b] -> App ()
addBookmarks xs = add xs >> add (concatMap getTags xs)
  where
      add bs = do
          bss <- groupBookmarks bs
          cs <- forM bss $ \bs' -> do
              bc <- readCloudWithDefault (head bs')
              addBookmarks' bs' bc
          mapM_ writeCloud cs

-- | Add a list of bookmarks to the cloud. It is assumed that
-- bookmarks are destined to the the given cloud after hashing.
-- Otherwise cloud is returned unmodified.
--
-- >>> let xs = sampleBMs
-- >>> (Right bss) <- appRun (groupBookmarks xs)
-- >>> let bs = head bss
-- >>> let b = head bs
-- >>> Right(bc) <- appRun (emptyBookmarkCloud b)
-- >>> Right(bc') <- appRun (addBookmarks' bs bc)
--
-- check insertion
--
-- >>> before <- mapM (`member` bc) bs
-- >>> or before
-- False
--
-- >>> after <- mapM (`member` bc') bs
-- >>> and after
-- True
addBookmarks' :: Bookmarkable b => [b]
              -> BookmarkCloud
              -> App BookmarkCloud
addBookmarks' = flip $ foldM $ flip addBookmark'

-- | Add bookmarks to the cloud. It is assumed that bookmarks
-- are destined to the the given cloud after hashing.
-- Otherwise cloud is returned unmodified.
--
-- >>> let xs = sampleBMs
-- >>> (Right bss) <- appRun (groupBookmarks xs)
-- >>> let bs = head bss
-- >>> let b = head bs
-- >>> Right(bc) <- appRun (emptyBookmarkCloud b)
-- >>> Right(bc') <- appRun (addBookmark' b bc)
--
-- check insertion
--
-- >>> before <- member b bc
-- >>> before
-- False
--
-- >>> after <- member b bc'
-- >>> after
-- True
addBookmark' :: Bookmarkable b => b
              -> BookmarkCloud
              -> App BookmarkCloud
addBookmark' b bc = getCurrentTime >>= \t -> ins t b bc
  where
      ins t = insertWith (updateTags t)
      updateTags t x' x | null diff = x
                        | otherwise = x & tags %~ (++ diff) &
                                      lastModified .~ t
        where diff = x' ^. tags \\ x ^. tags

-- | The expression @(updateBookmarkWith' p f b c)@ update
-- bookmark @b@ in the  cloud @c@.
-- If @(f b)@ is 'Nothing', @b@ is deleted. If it
-- is @('Just' y)@, then @b@ is replaced by @y@ when @(p y)@
-- is False, when @(p y) is 'True' @b@ is deleted.
-- It is assumed that Bookmarks  are destined to the the
-- given cloud after hashing.  Otherwise cloud is returned unmodified.
--
-- >>> let xs = sampleBMs
-- >>> (Right bss) <- appRun (groupBookmarks xs)
-- >>> let bs = head bss
-- >>> Right(bc) <- appRun (emptyBookmarkCloud (head bs))
-- >>> Right(bc') <- appRun (addBookmarks' bs bc)
--
-- check insertion
--
-- >>> before <- mapM (flip member bc) bs
-- >>> or before
-- False
--
-- >>> after <- mapM (flip member bc') bs
-- >>> and after
-- True
--
-- check removal
--
-- >>> let b1 = head bs
-- >>> Right(bc'') <- appRun (removeBookmark' b1 bc')
-- >>> after <- member b1 bc''
-- >>> after
-- False
--
-- check update
--
-- >>> let f b = b & tags %~ (++ ["test-tag"])
-- >>> Right (bc1) <- appRun (updateBookmark' (Just . f) b1 bc')
-- >>> after <- member b1 bc1
-- >>> after
-- True
-- >>> let check _ b = "test-tag" `elem` b ^.tags
-- >>> after <- memberWith check b1 bc1
-- >>> after
-- True
--
-- check extended update
--
-- >>> let c b = "test-tag" `elem` b ^.tags
-- >>> before <- member b1 bc1
-- >>> before
-- True
-- >>> Right (bc2) <- appRun (updateBookmarkWith' c (Just . f) b1 bc1)
-- >>> after <- member b1 bc2
-- >>> after
-- False
--
updateBookmarkWith' :: Bookmarkable b => (Bookmark -> Bool)
                    -> (Bookmark -> Maybe Bookmark)
                    -> b
                    -> BookmarkCloud
                    -> App BookmarkCloud
updateBookmarkWith' p f = update g'
  where
      g' = join . fmap g . f'
      f' = f . bookmarkableToBookmark
      g x | p x       = Nothing
          | otherwise = Just x

-- | Group bookmarks based on the clouds on which they get stored.
-- >>> let bs = sampleBMs
-- >>> let xs = sortBy (compare `on` snd) $ zip bs (map fpath bs)
-- >>> let expected = map (map fst) $ groupBy ((==) `on` snd) xs
-- >>> (Right actual) <- appRun (groupBookmarks bs)
-- >>> actual == expected
-- True
--
groupBookmarks :: Bookmarkable b => [b] -> App [[b]]
groupBookmarks bs = do
    xs <- forM bs $ \b -> do
              path <- cloudFilePath b
              return (b, path)
    let ys = groupBy f $ sortBy g xs
        f  = (==) `on` snd
        g  = compare `on` snd
    return $ map (map fst) ys

getTags :: Bookmarkable b => b -> [BookmarkTag]
getTags b = map mTag $ bookmarkTags b
  where
      mTag t = emptyBookmarkTag & tagTitle .~ t &
                tagDateAdded .~ bookmarkDateAdded b &
                tagLastModified .~ bookmarkLastModified b &
                tagBookmarkLinks .~ [bookmarkUri b]

mkBookmark :: String -> String -> [String] -> App Bookmark
mkBookmark bktitle bkurl bktags =  mkBookmark' <$> getCurrentTime
  where
      mkBookmark' t = emptyBookmark & title .~ pack bktitle &
                      dateAdded .~ t & lastModified .~ t &
                      uri .~ pack bkurl & tags .~ map pack bktags

mkTag :: String -> String -> App BookmarkTag
mkTag tgtitle url = mkTag' <$> getCurrentTime
  where
      mkTag' t = emptyBookmarkTag & tagTitle .~ pack tgtitle &
                 tagDateAdded .~ t & tagLastModified .~ t &
                 tagBookmarkLinks .~ [pack url]

bookmarkToBookmarkTag :: Bookmark -> BookmarkTag
bookmarkToBookmarkTag b = emptyBookmarkTag &
                          tagTitle .~ b ^. title &
                          tagDateAdded .~ b ^. dateAdded &
                          tagLastModified .~ b ^. lastModified &
                          tagBookmarkLinks .~ b ^. tags
