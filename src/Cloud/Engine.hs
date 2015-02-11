{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

----------------------------------------------------------------------
-- |
-- Module : Cloud.Engine
--
----------------------------------------------------------------------


module Cloud.Engine
    (
      -- * Hashed Cloud Type
      HCloud

      -- * Query cloud
    , isEmpty
    , search

      -- * Bookmark cloud manipulation
    , insertWith
    , update

      -- * Read/Write
    , readCloud
    , readCloudWithDefault
    , removeCloud
    , writeCloud

      -- * Group operations
    , applyOnList
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Function       (on)
import           Data.List           (groupBy, sortBy)
import           Data.Traversable    hiding (forM)

import           Cloud.App
import           Cloud.Cloud         (Cloud)
import qualified Cloud.Cloud         as Cloud
import           Cloud.Types


type HCloudF a = (FilePath, Cloud a)

newtype HCloud a = HCloud { getPair :: HCloudF a }
                   deriving (Show, Eq)

instance Functor HCloud where
    fmap f (HCloud a) = HCloud $ fmap (fmap f) a

instance Foldable HCloud where
    foldMap f (HCloud (_,c)) = foldMap f c

instance Traversable HCloud where
    traverse f (HCloud (p,c)) = (\c' -> HCloud (p, c')) <$> traverse f c

-- | Return 'True'if bookmark cloud is empty, 'False' otherwise.
isEmpty :: Storable a => HCloud a -> Bool
isEmpty = Cloud.isEmpty . snd . getPair

-- | Search a bookmarkable in the cloud. Returns @('Just' v)@
-- if present, or 'Nothing' otherwise.
search :: Storable b => b -> HCloud b -> Maybe b
search b = search' . snd . getPair
  where
      search' = Cloud.search b

-- | Insert with a function, combining new value and old value.
-- @insertWith f b bookmarkcloud@ will insert b into bookmarkcloud
-- if b does not exist in the bookmarkcloud. Otherwise, the
-- function will insert the value @f new_value old_value@ into
-- the bookmarkcloud.
insertWith :: Storable b =>(b -> b -> b)
           -> b
           -> HCloud b
           -> App (HCloud b)
insertWith f b hc@(HCloud(path, c)) = do
    ok <- (== path) <$> cloudFilePath b
    t  <- currentAppTime
    let go | ok = return $ HCloud (path, Cloud.insertWith t f b c)
           | otherwise = return hc
    go

-- | The expression @(update f b cloud)@ updates the value @b@
-- (if it is in the cloud). If @(f b)@ is 'Nothing', the element
-- is deleted. If it is @('Just' y)@, then @b@ is replaced by @y@.
update :: Storable b =>(b -> Maybe b) -> b -> HCloud b -> App (HCloud b)
update f b c = do
    t  <- currentAppTime
    return' $ update' t <$> getPair c
  where
      update' t = Cloud.update t f b
      return'   = return . HCloud

-- | The expression @applyOnList g f xs@ will apply the function
-- @f@ on list @xs@, @g@ is used to fetch the cloud corresponding
-- to an item @x@ in @xs@.
applyOnList :: Storable b => (b -> App (HCloud b))
            -> (b -> HCloud b -> App (HCloud b))
            -> [b]
            -> App [HCloud b]
applyOnList g f bs = do
    xss <- forM bs $ \b ->
        (b,) <$> cloudFilePath b

    let bss = map (map fst) $ group' xss

    forM bss $ \xs -> do
        c <- g $ head xs
        foldM (flip f) c xs
  where
      group' = groupBy ((==) `on` snd) . sortBy (compare `on` snd)

-- | Read a cloud from disk. If the operation
-- fails it returns an empty cloud.
readCloudWithDefault :: Storable b => b -> App (HCloud b)
readCloudWithDefault b = do
    x <- readCloud b
    p <- cloudFilePath b
    t <- currentAppTime
    let emptyCloud = HCloud(p, Cloud.emptyCloud t t)
    case x of
        Nothing -> return emptyCloud
        Just c  -> return c

-- | Read a cloud from disk.
readCloud :: Storable b => b -> App (Maybe (HCloud b))
readCloud b = do
    path <- cloudFilePath b
    x    <- readJSON path
    case x of
        Nothing -> return Nothing
        Just c  -> return $ Just $ HCloud(path, c)

-- | Remove the json file associate with the cloud.
removeCloud :: HCloud b -> App ()
removeCloud = removeJSON . fst . getPair

-- | Write a cloud back to disk.
writeCloud :: Storable b => HCloud b -> App ()
writeCloud = uncurry writeJSON . getPair
