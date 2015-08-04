
----------------------------------------------------------------------
-- |
-- Module : OptionParser
--
----------------------------------------------------------------------


module OptionParser
    (
      Options
    , Command (..)
    , Client (..)
    , programOptions
    , repoRoot
    , browser
    , optCommand
    , execOptions
    ) where


import           Options.Applicative
import           System.Directory


type Url = String
type Tag = String

data Client = FireFox
            | Unknown
            deriving (Show)

data Options = Options
    { repoRoot   :: FilePath
    , browser    :: Client
    , optCommand :: Command
    } deriving (Show)

data Command = InitRepo
             | AddUrl String Url [Tag]
             | SearchUrl Url
             | SearchTag Tag
             | AttachTag Tag Url
             | RemoveBookmark Url
             | RemoveTag Tag Url
             | ImportBookmarks [FilePath]
             | SyncBookmarks
               deriving (Show)

execOptions :: (Options -> IO ()) -> IO ()
execOptions run = do
    dir <- getCurrentDirectory

    let opts = info (helper <*> programOptions dir)
               ( fullDesc
               <> progDesc "Allow manage bookmarks and tags"
               <> header "command line utility to manage bookmarks."
               )

    opt <- execParser opts
    run opt

programOptions :: FilePath -> Parser Options
programOptions root = Options <$> rootDirOption root
                      <*> clientOption
                      <*> commands

commands :: Parser Command
commands = subparser ( command "init" (info initOptions
           ( progDesc "Initialize the repository" ))
           <> command "add" (info addUrlOptions
           ( progDesc "Add a url to the repository" ))
           <> command "search-url" (info searchUrlOptions
           ( progDesc "Search a url in the repository" ))
           <> command "search-tag" (info searchTagOptions
           ( progDesc "Search a tag in the repository" ))
           <> command "attach-tag" (info attachTagOptions
           ( progDesc "Search a tag in the repository" ))
           <> command "remove-url" (info removeBookmarkOptions
           ( progDesc "Remove a url from the repository" ))
           <> command "remove-tag" (info removeTagOptions
           ( progDesc "Remove a tag attached to a url" ))
           <> command "import" (info importOptions
           ( progDesc "Import  bookmarks from a collection of files" ))
           <> command "sync" (info syncOptions
           ( progDesc "Sync repository with remote server")))

initOptions :: Parser Command
initOptions = pure InitRepo

syncOptions :: Parser Command
syncOptions = pure SyncBookmarks

importOptions :: Parser Command
importOptions = ImportBookmarks <$> files

toClient :: String -> Client
toClient "firefox" = FireFox
toClient c         = error ("Unknown Client " ++ c)

clientOption :: Parser Client
clientOption = toClient <$> strOption
          ( long "client"
          <> short 'c'
          <> metavar "CLIENT"
          <> value "firefox"
          <> help "client from which the bookmarks are importing"
          )

files :: Parser [FilePath]
files = some (strArgument $ metavar "FILES...")

removeTagOptions :: Parser Command
removeTagOptions = RemoveTag <$> tag <*> url

removeBookmarkOptions :: Parser Command
removeBookmarkOptions = RemoveBookmark <$> url

attachTagOptions :: Parser Command
attachTagOptions = AttachTag <$> tag <*> url

addUrlOptions :: Parser Command
addUrlOptions = AddUrl <$> strArgument' "TITLE"
                <*> url
                <*> many (strArgument' "TAGS...")

searchUrlOptions :: Parser Command
searchUrlOptions = SearchUrl <$> url

searchTagOptions :: Parser Command
searchTagOptions = SearchTag <$> tag

strArgument' :: String -> Parser String
strArgument' name = strArgument $ metavar name

url :: Parser Url
url = strArgument $ metavar "URL"

tag :: Parser Tag
tag = strArgument $ metavar "TAG"

rootDirOption :: FilePath -> Parser FilePath
rootDirOption root = strOption
          ( long "root"
          <> metavar "ROOT_DIR"
          <> value root
          <> help "root directory of the repository"
          )
