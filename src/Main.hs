{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Error
import           Control.Applicative         ((<$>))
import           Data.Char                   (toLower)
import           Control.Monad
import           System.Directory
import           Control.Monad.Reader

import Api.Core
import BookmarkCloud.Config
import FirefoxBookmark
import App


initRepo :: App ()
initRepo = do

    cfg <- config <$> ask
    root <- rootDir <$> ask

    mapM_ createDir [ bookmarkCloudDirectoryPath cfg root
                    , tagCloudDirectoryPath cfg root
                    ]

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

main :: IO ()
main = do
    e <- getEnvironment "/home/valyakuttan/Downloads"
    void $ runApp initRepo e
    r <- runApp (addBookmarks sampleBookmarks) e
    case r of
        Left msg -> putStrLn msg
        Right _  -> putStrLn "ok"

