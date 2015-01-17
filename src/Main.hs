
module Main where


import Api.Cloud
import Api.Bookmark
import App
import FirefoxBookmark


main :: IO ()
main = do
    e <- getDefaultAppEnvironment "/home/valyakuttan/Downloads"
    let action = initRepo >> importBookmarks sampleBookmarks
    r <- runApp action e
    case r of
        Left msg -> putStrLn msg
        Right _  -> putStrLn "ok"

initRepo :: App ()
initRepo = do
    bookmarkDir <- cloudDirectory Book
    tagDir      <- cloudDirectory Tag
    mapM_ createAppDirectory [bookmarkDir, tagDir]

appRun :: App a -> IO (Either String a)
appRun a = do
    e <- getDefaultAppEnvironment "/home/valyakuttan/Downloads"
    runApp a e


