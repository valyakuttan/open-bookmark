{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Char        (toLower)
import           Data.Function    (on)
import           Data.List        (groupBy, sortBy, (\\))
import           System.Directory

import           Api.Core
import           FirefoxBookmark


initRepo :: App ()
initRepo = do

    bookdir <- cloudDirectory Book
    tagdir <- cloudDirectory Tag

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

groupBookmarks :: Bookmarkable b => [b] -> App [[b]]
groupBookmarks bs = do
    xs <- forM bs $ \b -> do
              path <- cloudFilePath b
              return (b, path)
    let ys = groupBy f $ sortBy g xs
        f  = (==) `on` snd
        g  = compare `on` snd
    return $ map (map fst) ys

addBookmark :: Bookmarkable b => b -> App ()
addBookmark b = addBookmarks [b]

addBookmarks :: Bookmarkable b => [b] -> App ()
addBookmarks bs = addBookmarks' bs >> addBookmarks' ts
  where
    ts = concatMap getTags bs

addBookmarks' :: Bookmarkable b => [b] -> App ()
addBookmarks' bs = do

    time <- getCurrentTime
    xss <- groupBookmarks bs
    cs <- mapM (readCloudWithDefault . head) xss

    let update' = updateBookmarks time
        ins     = flip $ insertWith update'

    cs' <- zipWithM (foldM ins) cs xss

    let write' c c' | c == c' = return ()
                    | otherwise = writeCloud c'

    zipWithM_ write' cs cs'

updateBookmarks :: POSIXMicroSeconds ->  Bookmark -> Bookmark -> Bookmark
updateBookmarks ctime b' b | null diff = b
                           | otherwise = b & tags %~ (++ diff) &
                                         lastModified .~ ctime
  where diff = b' ^. tags \\ b ^. tags

getTags :: Bookmarkable b => b -> [BookmarkTag]
getTags b = map mkTag $ bookmarkTags b
  where
      mkTag t = BookmarkTag t da dm [bookmarkUri b]
      da = bookmarkDateAdded b
      dm = bookmarkLastModified b

main :: IO ()
main = do
    e <- getDefaultAppEnvironment "/home/valyakuttan/Downloads"

    let action = initRepo >> addBookmarks sampleBookmarks

    r <- runApp action e
    case r of
        Left msg -> putStrLn msg
        Right _  -> putStrLn "ok"
