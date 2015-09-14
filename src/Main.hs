
module Main where


import           Control.Monad
import qualified Data.ByteString.Lazy as B (readFile)

import qualified Cloud.Config         as Cfg
import           Api.Cloud
import           Api.Core
import           Client.Firefox
import           OptionParser


main :: IO ()
main = execOptions run

appRun :: FilePath -> App a -> IO ()
appRun root a = do
    e <- defaultAppEnvironment root $ Cfg.configFilePath root
    r <- runApp a e
    case r of
        Right _  -> return ()
        Left msg -> putStrLn msg

run :: Options -> IO ()
run opt = case optCommand opt of
    InitRepo              -> appRun' initRepo
    AddUrl title url tags -> appRun' $ addUrl title url tags
    SearchUrl url         -> appRun' $ searchUrl url
    SearchTag tag         -> appRun' $ searchTag' tag
    AttachTag tag url     -> appRun' $ attachTag' tag url
    RemoveBookmark url    -> appRun' $ removeUrl url
    RemoveTag tag url     -> appRun' $ removeTag' tag url
    ImportBookmarks fs    -> appRun' $ import' fs
    SyncBookmarks         -> appRun' syncBookmarks
  where
      import' = importBookmarks' $ browser opt
      appRun' = appRun (repoRoot opt)

syncBookmarks :: App ()
syncBookmarks = appPrint "Bookmarks synced.."

importBookmarks' :: Client -> [FilePath] -> App ()
importBookmarks' c fs = forM_ fs $ \f -> do
    xs <- performIO $ B.readFile f
    case parseJson c xs of
        Right bs -> do
             (bcs,tcs) <- import' bs
             mapM_ writeCloud bcs
             mapM_ writeCloud tcs
             appPrint $ "Successfully imported bookmarks from " ++ f
        Left msg -> appPrint msg
  where
      import' = importBookmarks readCloudWithDefault readCloudWithDefault
      parseJson FireFox = jsonToBookmarks
      parseJson _       = error "Unknown Client"

removeTag' :: String -> String -> App ()
removeTag' tag url = do
    r <- remove' tag url
    case r of
        Just (bc,tc) -> do
            writeCloud bc
            writeCloud tc
            appPrint $ "Removed tag : " ++ tag ++ " from : " ++ url
        Nothing -> appPrint $ "Removing tag : " ++ tag ++
                   "from :" ++ url ++ " failed.."
  where
      remove' = removeTag readCloudWithDefault readCloudWithDefault

removeUrl :: String -> App ()
removeUrl url = do
    r <- remove' url
    case r of
        Just (bc,tcs) -> do
            writeCloud bc >> mapM_ writeCloud tcs
            appPrint $ "Removed url: " ++ url
        Nothing -> appPrint $ "Removing " ++ url ++ " failed.."
  where
      remove' = removeBookmark readCloudWithDefault readCloudWithDefault

attachTag' :: String -> String -> App ()
attachTag' tag url = do
    r <- attach' tag url
    case r of
        Just (bc,tc) -> do
            writeCloud bc
            writeCloud tc
            appPrint $ "Tagged " ++ url ++ " with " ++ tag
        Nothing -> appPrint "Tagging failed.."
  where
      attach' = attachTag readCloudWithDefault readCloudWithDefault

addUrl :: String -> String -> [String] -> App ()
addUrl title url tags = do
   (bc,tcs) <- add' title url tags
   writeCloud bc >> mapM_ writeCloud tcs

   appPrint $ "Bookmarked " ++ url
  where
      add' = addBookmark readCloudWithDefault readCloudWithDefault

searchUrl :: String -> App ()
searchUrl url = do
    r <- searchBookmark readCloudWithDefault url
    case r of
        Just b  -> appPrint $ show b
        Nothing -> appPrint $ url ++ " not found.."

searchTag' :: String -> App ()
searchTag' tag = do
    r <- searchTag readCloudWithDefault tag
    case r of
        Just b  -> appPrint $ show b
        Nothing -> appPrint $ tag ++ " not found.."

initRepo :: App ()
initRepo = do
    root <- currentAppRoot
    let xs = [ Cfg.bookmarkDirectoryPath root
             , Cfg.tagDirectoryPath root
             , Cfg.configDirectoryPath root
             ]
        cfgFile = Cfg.configFilePath root
    mapM_ createAppDirectory xs
    writeJSON cfgFile Cfg.defaultConfig

    appPrint ("Bookmark repo initialized at : " ++ root)
    let msg = "Edit config file at : " ++
              cfgFile ++
              " before proceeding.."
    appPrint msg

appPrint :: String -> App ()
appPrint = performIO . putStrLn
