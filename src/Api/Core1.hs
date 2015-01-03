{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module : Api.Core
--
----------------------------------------------------------------------


module Api.Core
    (
      addBookmark
    , addBookmarks
    , addTag
    , searchBookmark
    , searchTag
    ) where


import           Control.Applicative         ((<$>))
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.List                   (foldl')
import qualified Data.Map                    as M
import           Data.Text                   (pack, unpack)


import           App
import           BookmarkCloud.Config
import           BookmarkCloud.Core.Bookmark
import           BookmarkCloud.Core.Tag
import           BookmarkCloud.Utils


data TempTag
    = TempTag !String !POSIXMicroSeconds !POSIXMicroSeconds ![Url]


data TempBM
    = TempBM !String !POSIXMicroSeconds !POSIXMicroSeconds !Url ![Tag]

type Tag = String
type Url = String

-- | Serach a url in the cloud.
-- This will return @('Just' b)@ if the corresponding
-- bookmark is found otherwise return 'Nothing'.
searchBookmark :: Url -> App (Maybe BookmarkJSON)
searchBookmark url = do
    t    <- currentTime <$> ask
    let b = TempBM "" t t url []
    search b bookmarkCloudFilePath lookupBookmark

-- | Serach a tag in the cloud.
-- This will return @('Just' t)@ if the corresponding
-- record is found otherwise return 'Nothing'.
searchTag :: Tag -> App (Maybe TagJSON)
searchTag tg =  do
    t    <- currentTime <$> ask
    let b = TempTag tg t t []
    search b tagCloudFilePath lookupTag

-- | Attach a tag to the given url.
addTag :: Tag -> Url -> App ()
addTag tag url = addBookmark "" url [tag]

-- | Add a bookmark to the cloud along with its tags.
addBookmark :: String -> Url -> [Tag] -> App ()
addBookmark title url tags = do
    time <- currentTime <$> ask
    addBookmarks [TempBM title time time url tags]

-- | Add a list of bookmarkable to the bookmark clouds.
-- This will also modify the tag clouds to include tags
-- associated with bookmarks.
addBookmarks :: (Bookmarkable b) => [b] -> App ()
addBookmarks bookmarks = do

    t <- currentTime <$> ask
    bc <- bCloud <$> ask
    tc <- tCloud <$> ask
    (bs,ts) <- parseBookmarks bookmarks

    updateClouds (readCloud bc) (foldl' $ flip $ insertBookmark t) bs
    updateClouds (readCloud tc) (foldl' $ flip $ insertTag t) ts
  where
      readCloud d
          = performIO . readJSON >=> return . either (const d) id

-- | Update clouds contents by performing the update operation.
-- If the cloud get modified that will be written back
-- to JSON files.
updateClouds :: (ToJSON c, Eq c) => (FilePath -> App c)
             -> (c -> a -> c)
             -> [(FilePath, a)]
             -> App ()
updateClouds retrieve update xs = do

    let (fs,as) = unzip xs

    cs <- mapM retrieve fs

    let f (a,b,c) zs | b /= c    = (a,b) : zs
                     | otherwise = zs
        cs' = zipWith update cs as

    mapM_ (uncurry writeJSON) $ foldr f [] $ zip3 fs cs' cs

-- | Parse a list of bookmarks to two associative lists where
-- the first list map bookmark cloud file to bookmarks and the
-- second list map tag to bookmark cloud files.
parseBookmarks :: Bookmarkable b => [b]
               -> App ([(FilePath, [b])], [(FilePath, [TempTag])])
parseBookmarks bs = do

    cfg <- config   <$> ask
    root <- rootDir <$> ask

    let xs   = (\b -> (bf b, b, bookmarkTags b)) <$> bs
        bf = bookmarkCloudFilePath cfg root

    let (mb, m) = execState (go xs) (M.empty, M.empty)

    time <- currentTime <$> ask

    let mt  = execState (parseTags ts) M.empty
        ts = (\t -> (g t, t)) <$> ys
        g  = tagCloudFilePath cfg root
        ys = uncurry (mkTg time) <$> M.assocs m

    return (M.assocs mb, M.assocs mt)
  where
    go xs = forM_ xs $ \(bf, b, ts) -> do
        _1 %= M.insertWith (++) bf  [b]
        forM_ ts $ \t ->
            _2 %= M.insertWith (++) t [url b]
    parseTags ts = forM_ ts $ \(tf, t) ->
        id %= M.insertWith (++) tf [t]
    mkTg time title = TempTag (unpack title) time time
    url b = unpack . fromMaybe "" $ bookmarkUri b

-- | Generalized search for the cloud.
search :: FromJSON c => b
       -> (Config -> FilePath -> b -> FilePath)
       -> (b -> c -> Maybe a)
       -> App (Maybe a)
search a pf lf = do
    cfg  <- config <$> ask
    root <- rootDir <$> ask
    r <- performIO (readJSON $ pf cfg root a)
    either (lift . left) (return . lf a) r

instance Bookmarkable TempBM where
  bookmarkTitle (TempBM t _ _ _ _) = Just $ pack t
  bookmarkDateAdded (TempBM _ d  _ _ _) = Just d
  bookmarkLastModified (TempBM _ _ m _ _) = Just m
  bookmarkUri (TempBM _ _ _ u _) = Just $ pack u
  bookmarkTags (TempBM _ _ _ _ ts) = map pack ts

instance Taggable TempTag where
  tagTitle (TempTag t _ _ _)            = pack t
  tagDateAdded (TempTag  _ d _ _)      = d
  tagLastModified (TempTag _ _ d  _)   = d
  tagBookmarkLinks (TempTag _ _ _  ls) = map pack ls
