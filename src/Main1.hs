module Main where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Text as T
import System.Directory hiding (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as B

import Config
import OptionParser
import Bookmark
import FirefoxBookmarks


-- | Initialize the setup
initialize :: FilePath -> IO Config
initialize config = do
  configExists <- doesFileExist config
  if configExists then readJSON config error return else initRepo config

-- | Initialize bookmark repository by creating directory structure
--   return default config info
initRepo :: FilePath -> IO Config
initRepo config =
      let f = (takeDirectory config </>) . T.unpack . ($ defaultConfig)
          (datadir : bookmarsdir : tagsdir : _)
            = f <$> [ dataDirectory
                    , bookmarksDirectory
                    , tagsDirectory
                    ]
          createdirs = mapM_ (createDirectoryIfMissing True)
      in createdirs [datadir, bookmarsdir, tagsdir] >> return defaultConfig
   
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing _ = putStrLn . (++ " created..")

readJSON :: FromJSON a => FilePath -> (String -> IO b) -> (a -> IO b) -> IO b
readJSON path err act = B.readFile path >>= either err act . eitherDecode

writeJSON :: (Show a, ToJSON a) => FilePath -> a -> IO ()
writeJSON path a = putStrLn ("written " ++ show a ++ " to " ++ path)
--B.writeFile path $ encode a

configFile :: FilePath
configFile = "config.json"

run :: Options -> IO ()
run (OptionsWithCommand (ImportFromFF xs) (RootDirectory root)) = do
  let cfgPath = root </> configFile
  cfg <- initialize cfgPath

  let cloudPath = bookmarkCloudPath root cfg
  bcloud <- initializeBookmarkCloud cloudPath
  
  let bms = bcBookmarks bcloud
      bid = lastBookmarkId cfg
  (fbs, bid') <- importFirefoxBookmarks bid xs
  let bcloud' = bcloud {bcBookmarks = bms ++ fbs}
      cfg'    = cfg {lastBookmarkId = bid'}
 
  writeJSON cloudPath bcloud'
  writeJSON cfgPath cfg'
run o = print o

initializeBookmarkCloud :: FilePath -> IO BookmarkCloud
initializeBookmarkCloud cloudPath = do
  cloudExists <- doesFileExist cloudPath
  if cloudExists
    then readJSON cloudPath error return
    else return defaultBookmarkCloud

bookmarkCloudPath :: FilePath -> Config -> FilePath
bookmarkCloudPath root cfg =
  root </> bookmarkDir </> (prefix  ++ index ++ ".json")
  where
    (bookmarkDir : prefix : index : _)
      = map f [ bookmarksDirectory
              , bookmarkCloudPrefix
              , T.pack . show .lastBookmarkCloudIndex
              ]
    f  = T.unpack . ($ cfg)

importFirefoxBookmarks :: BookmarkId -> [FilePath] -> IO ([Bookmark], BookmarkId)
importFirefoxBookmarks bid = foldM combine ([], bid)
  where
    combine (bs, i) path = do
      let retrieve = either (const ([],i)) id . firefoxJSONToBookmarks i
      (bs',i') <- retrieve  <$> B.readFile path
      return (bs ++ bs', i')
                       
main :: IO ()
main = optionParser >>= run
