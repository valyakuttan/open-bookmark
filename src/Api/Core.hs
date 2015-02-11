{-# LANGUAGE TupleSections #-}

----------------------------------------------------------------------
-- |
-- Module : Api.Core
--
----------------------------------------------------------------------


module Api.Core
     (
       -- * Query cloud
       isEmpty
     , searchBookmark
     , searchTag

       -- * Modify cloud
     , importBookmarks
     , addBookmark
     , attachTag
     , removeBookmark
     , removeTag

       -- * Read/Write
     , readCloud
     , readCloudWithDefault
     , removeCloud
     , writeCloud
     )  where


import           Control.Applicative
import           Control.Lens
import           Data.List           ((\\))
import           Data.Text           (Text, pack)

import           Api.Cloud
import           Cloud.Engine


type BookmarkCloud = HCloud Bookmark
type TagCloud      = HCloud Tag

-- | Search a url in the bookmark cloud. If found
-- return @'App'('Just' bookmark)@, otherwise
-- return 'App Nothing'.
searchBookmark :: (Bookmark -> App BookmarkCloud)
               -> String
               -> App (Maybe Bookmark)
searchBookmark readBookmarkCloud url = search b <$> readBookmarkCloud b
  where b = emptyBookmark & bookmarkUrl .~ pack url

-- | Search a tag name in the bookmark cloud. If found
-- return @App('Just' tag)@, otherwise return 'App Nothing'.
searchTag ::  (Tag -> App TagCloud)
          -> String
          -> App (Maybe Tag)
searchTag readTagCloud t = search tg <$> readTagCloud tg
  where
      tg = emptyTag & tagTitle .~ pack t

-- | Bookmark a url and attch bookmarkTitle and tags to it.
addBookmark :: (Bookmark -> App BookmarkCloud)
            -> (Tag -> App TagCloud)
            -> String
            -> String
            -> [String]
            -> App (BookmarkCloud, [TagCloud])
addBookmark readBookmarkCloud readTagCloud title url tgs = do
    bc  <- createBookmark readBookmarkCloud title url tgs
    tcs <- mapM createTag' tgs
    return (bc, tcs)
  where createTag' t = createTag readTagCloud t url

-- | Create a bookmark entry in the cloud with given attributes.
createBookmark :: (Bookmark -> App BookmarkCloud)
               -> String
               -> String
               -> [String]
               -> App BookmarkCloud
createBookmark readBookmarkCloud bbookmarkTitle url tgs = do
    b   <- mkBookmark bbookmarkTitle url tgs
    bc  <- readBookmarkCloud b
    addBookmark' b bc

-- | Create a tag entry in the cloud with given attributes.
createTag ::(Tag -> App TagCloud)
          -> String
          -> String
          -> App TagCloud
createTag readTagCloud tag url = do
    t   <- mkTag tag url
    tc  <- readTagCloud t
    addTag' t tc

-- | Attach tag to an existing bookmark.
attachTag :: (Bookmark -> App BookmarkCloud)
          -> (Tag -> App TagCloud)
          -> String
          -> String
          -> App (Maybe (BookmarkCloud, TagCloud))
attachTag readBookmarkCloud readTagCloud tag url = do
    x  <- mkBookmark "" url [tag]
    bc <- readBookmarkCloud x
    case search x bc of
        Just _  -> do
            bc' <- addBookmark' x bc
            tc  <- createTag readTagCloud tag url
            return $ Just (bc',tc)
        Nothing -> return Nothing

-- | Remove a bookmark enty associated with @url@ from the cloud.
-- Also it will update tag entries which are attached to this
-- @url@.
removeBookmark :: (Bookmark -> App BookmarkCloud)
               -> (Tag -> App TagCloud)
               -> String
               -> App (Maybe (BookmarkCloud, [TagCloud]))
removeBookmark readBookmarkCloud readTagCloud url = do
    x  <- mkBookmark "" url []
    bc <- readBookmarkCloud x
    case search x bc of
        Nothing -> return Nothing
        Just b  -> do
            tcs <- remove' $ getTags b
            bc' <- removeBookmark' b bc
            return $ Just (bc', tcs)
  where
      removeBookmark' = update (const Nothing)
      remove'         = applyOnList readTagCloud removeUrl'
      removeUrl'      = removeUrlFromTag' $ pack url

-- | Remove tag attached to the url by removing it from from the
-- bookmark entry. Also it will remove url from the links of tag entry.
removeTag :: (Bookmark -> App BookmarkCloud)
          -> (Tag -> App TagCloud)
          -> String
          -> String
          -> App (Maybe (BookmarkCloud, TagCloud))
removeTag readBookmarkCloud readTagCloud tag url = do
    x  <- mkBookmark "" url []
    bc <- readBookmarkCloud x
    case search x bc of
        Nothing -> return Nothing
        Just b -> do
            y  <- mkTag tag ""
            tc <- readTagCloud y
            bc' <- removeTagFromBookmark' (pack tag) b bc
            case search y tc of
                Nothing -> return $ Just (bc', tc)
                Just t  -> do
                    tc' <- removeUrlFromTag' (pack url) t tc
                    return $ Just (bc', tc')

-- | Remove url from a tag's links. If links are
-- empty the bookmark tag is removed from bookmark cloud.
removeUrlFromTag' :: Text
                  -> Tag
                  -> TagCloud
                  -> App TagCloud
removeUrlFromTag' url = update f
  where
      check b = b ^. bookmarkUrls == [url]
      f b | check b   = Nothing
          | otherwise = Just $ b & bookmarkUrls %~ filter (/= url)

-- | Remove tag from bookmark's tag list.
removeTagFromBookmark' :: Text
                       -> Bookmark
                       -> BookmarkCloud
                       -> App BookmarkCloud
removeTagFromBookmark' tg = update (Just . f)
  where f x = x & bookmarkTags %~ filter (/= tg)

-- | Import a list of bookmarkable to bookmark cloud.
--
importBookmarks :: ToBookmark b => (Bookmark -> App BookmarkCloud)
                -> (Tag -> App TagCloud)
                -> [b]
                -> App ([BookmarkCloud], [TagCloud])
importBookmarks readBookmarkCloud readTagCloud xs =
    (,) <$> insertB bs <*> insertT (concatMap getTags bs)
  where
      bs      = map toBookmark xs
      insertB = applyOnList readBookmarkCloud addBookmark'
      insertT = applyOnList readTagCloud addTag'

-- | Add tag to the cloud. It is assumed that this tag
-- is destined to the the given cloud after hashing.
-- Otherwise cloud is returned unmodified.
addTag' :: Tag
        -> TagCloud
        -> App TagCloud
addTag' tg tc = currentAppTime >>= \t -> ins t tg tc
  where
      ins t = insertWith (updateTags t)
      updateTags t x' x | null diff = x
                        | otherwise = x &
                          bookmarkUrls %~ (++ diff) &
                          tagLastModified .~ t
        where diff = x' ^. bookmarkUrls \\ x ^. bookmarkUrls

-- | Add bookmark to the cloud. It is assumed that thsi bookmark
-- is destined to the the given cloud after hashing.
-- Otherwise cloud is returned unmodified.
addBookmark' :: Bookmark
             -> BookmarkCloud
             -> App BookmarkCloud
addBookmark' b bc = currentAppTime >>= \t -> ins t b bc
  where
      ins t = insertWith (updateTags t)
      updateTags t x' x | null diff = x
                        | otherwise = x &
                          bookmarkTags %~ (++ diff) &
                          bookmarkLastModified .~ t
        where diff = x' ^. bookmarkTags \\ x ^. bookmarkTags

getTags :: Bookmark -> [Tag]
getTags b = map mTag $ b ^. bookmarkTags
  where
      mTag t = emptyTag & tagTitle .~ t &
                tagDateAdded .~ b ^. bookmarkDateAdded &
                tagLastModified .~ b ^. bookmarkLastModified &
                bookmarkUrls .~ [b ^. bookmarkUrl]

mkBookmark :: String -> String -> [String] -> App Bookmark
mkBookmark bkbookmarkTitle bkurl bktags =  mkBookmark' <$> currentAppTime
  where
      mkBookmark' t = emptyBookmark &
                      bookmarkTitle .~ pack bkbookmarkTitle &
                      bookmarkDateAdded .~ t &
                      bookmarkLastModified .~ t &
                      bookmarkUrl .~ pack bkurl &
                      bookmarkTags .~ map pack bktags

mkTag :: String -> String -> App Tag
mkTag tag url = mkTag' <$> currentAppTime
  where
      mkTag' t = emptyTag &
                 tagTitle .~ pack tag &
                 tagDateAdded .~ t &
                 tagLastModified .~ t &
                 bookmarkUrls .~ [pack url]
