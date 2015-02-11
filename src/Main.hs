
module Main where


import           Api.Cloud
import           Api.Core
import           FirefoxBookmark


main :: IO ()
main = do
    e <- defaultAppEnvironment "/home/valyakuttan/Downloads"
    let action = do
          initRepo
          (bs,ts) <- importBookmarks' sampleBookmarks
          mapM_ writeCloud bs
          mapM_ writeCloud ts

    r <- runApp action e
    case r of
        Left msg -> putStrLn msg
        Right _  -> putStrLn "ok"
  where
    importBookmarks' = importBookmarks
                       readCloudWithDefault
                       readCloudWithDefault

initRepo :: App ()
initRepo = do
    bookmarkDir <- bookmarkDirectoryPath
    tagDir      <- tagDirectoryPath
    mapM_ createAppDirectory [bookmarkDir, tagDir]

appRun :: App a -> IO (Either String a)
appRun a = do
    e <- defaultAppEnvironment "/home/valyakuttan/Downloads"
    runApp a e
