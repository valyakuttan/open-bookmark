{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Error
import           Control.Monad
import           Data.Char        (toLower)
import           Data.Text        (Text)
import           System.Directory

import           Api.Core
import           FirefoxBookmark


data TempTag = TempTag
    { title        :: !Text
    , dateAdded    :: !POSIXMicroSeconds
    , lastModified :: !POSIXMicroSeconds
    , urls         :: ![Text]
    }

instance Taggable TempTag where
    tagTitle        = title
    tagDateAdded    = dateAdded
    tagLastModified = lastModified
    tagBookmarkLinks = urls

initRepo :: App ()
initRepo = do

    bookdir <- bookmarkCloudDirectory
    tagdir <- tagCloudDirectory

    mapM_ createDir [ bookdir, tagdir]

createDir :: FilePath -> App ()
createDir path = do
  exists <- performIO $ doesDirectoryExist path
  unless exists $ do
    reply <- readConsole ("Create Directory " ++ path ++ " [Y]es/No : ")
    let ok = toLower (headDef 'y' reply) == 'y'
    when ok $ performIO $ createDirectoryIfMissing True path

readConsole :: String -> App String
readConsole msg = do
  performIO $ putStrLn msg
  performIO getLine

addBookmark :: Bookmarkable b => b -> App ()
addBookmark b = insB >> mapM_ insT (getTags b)
  where
      insB = readBookmarkCloudWithDefault b
               >>= insertBookmark b
               >>= writeBookmarkCloud
      insT t = readTagCloudWithDefault t
                  >>= insertTag t
                  >>= writeTagCloud

getTags :: Bookmarkable b => b -> [TempTag]
getTags b = map mkTag $ bookmarkTags b
  where
      mkTag t = TempTag t da dm [bookmarkUri b]
      da = bookmarkDateAdded b
      dm = bookmarkLastModified b

main :: IO ()
main = do
    e <- getDefaultAppEnvironment "/home/valyakuttan/Downloads"
    void $ runApp initRepo e
    let b = head sampleBookmarks
    r <- runApp (addBookmark b) e
    case r of
        Left msg -> putStrLn msg
        Right _  -> putStrLn "ok"
