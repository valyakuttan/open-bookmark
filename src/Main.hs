
module Main where

import Control.Applicative ((<$>))
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import System.Directory
import System.FilePath ((</>))

import Bookmark
import Config
import FirefoxBookmark
import Tag
import Utils

type App = EitherT String IO

runApp :: App a -> IO (Either String a)
runApp = runEitherT

addBookmarks :: (Bookmarkable b) => FilePath -> [b] -> App ()
addBookmarks root bs = do

    let cfg = defaultConfig
    initRepo $ map T.unpack [bookmarksDirectory cfg, tagsDirectory cfg]

    let (bookmarks,tgs) = parseBookmarks bs
    
        bookmarkCloudKeys = fst <$> bookmarks
        tagCloudKeys = fst <$> tgs
  
    let mkPath as i =
          let (dir:prefix:_) = T.unpack . ($ cfg) <$> as
          in root </> dir </> (prefix ++ show i ++ ".json")

        bookmarkFiles = foldr f1 [] bookmarkCloudKeys
        f1 = (:) . mkPath [bookmarksDirectory, bookmarkCloudPrefix]

        tagFiles = foldr f2 [] tagCloudKeys
        f2 = (:) . mkPath [tagsDirectory, tagCloudPrefix]


    let readCloud d path = do
          exists <- performIO $ doesFileExist path  
          if exists
          then do
              j <- eitherDecode <$> readJSON path
              hoistEither j
          else return d
 
    let zipBookmarks = zip (map snd bookmarks)
    cs <- zipBookmarks <$> mapM (readCloud defaultBookmarkCloud) bookmarkFiles
    ts <- zip (map snd tgs) <$> mapM (readCloud defaultTagCloud) tagFiles

    let cs' = zip bookmarkFiles $ map (encode . uncurry insertBookmarks) cs
        ts' = zip tagFiles $ map (encode . uncurry insertTags) ts

    forM_ cs' $ uncurry writeJSON
    forM_ ts' $ uncurry writeJSON

parseBookmarks ::
  (Bookmarkable b) => [b] -> ([(Int, [BookmarkJSON])], [(Int, [TagJSON])])
parseBookmarks = convert . foldr parse (M.empty, M.empty)
  where
    parse :: (Bookmarkable b) => b
          -> (Map Int [BookmarkJSON], Map Int [TagJSON])
          -> (Map Int [BookmarkJSON], Map Int [TagJSON])
    parse b (bmap, tmap) =
      let maxN = maximumNumberOfClouds defaultConfig
          bm = makeBookmark b
          n  = bookmarkHash b `mod` maxN
          bmap' = M.insertWith (++) n [bm] bmap
          da = bookmarkDateAdded b
          dm = bookmarkLastModified b
          mkTag t = makeTag t da dm [bookmarkUri b]
          m t = textHash t `mod` maxN
          insertTag t = M.insertWith (++) (m t) [mkTag t]
          tmap' = foldr insertTag tmap $ bookmarkTags b
      in (bmap', tmap')

    convert :: (Map Int [BookmarkJSON], Map Int [TagJSON])
            -> ([(Int, [BookmarkJSON])], [(Int, [TagJSON])])
    convert (a,b) = (M.assocs a, M.assocs b)
  
initRepo :: [FilePath] -> App ()
initRepo = mapM_ createDir

createDir :: FilePath -> App ()
createDir path = do
  exists <- performIO $ doesDirectoryExist path
  unless exists $ do
    reply <- readConsole ("Create Directory " ++ path ++ " [Y]es/No : ")
    let ok = toLower (headDef 'y' reply) == 'y'
    when ok $ performIO $ createDirectory' path

createDirectory' :: FilePath -> IO ()
createDirectory' = putStrLn

readConsole :: String -> App String
readConsole msg = do
  performIO $ putStrLn msg
  performIO getLine
  
writeJSON :: FilePath -> ByteString -> App ()
writeJSON path _ = --performIO . B.writeFile path
  performIO $ putStrLn ("writing file : " ++ path)
  
readJSON :: FilePath -> App ByteString
readJSON path = do -- performIO . B.readFile
  performIO $ putStrLn ("reading file : " ++ path)
  return B.empty
  
performIO :: IO a -> App a
performIO = fmapLT show . tryIO

main :: IO ()
main = undefined
