
module OptionParser (
   Command (..)
  ,  Options (..)
  , Option (..)
  , optionParser
  ) where

import Options.Applicative

type Title = String
type Url   = String
type Tag   = String

data Options = OptionsWithCommand Command Option
             deriving (Show)

data Option = RootDirectory FilePath
            deriving (Show)

data Command = Add Title Url [Tag]
             | Remove Title
             | ImportFromFF [FilePath]
               deriving (Show)

optionParser :: IO Options
optionParser = execParser (parseOptions `withInfo` "Bookmark a url")

parseOptions :: Parser Options
parseOptions = OptionsWithCommand <$> parseCommand <*> parseOption


parseCommand :: Parser Command
parseCommand =
  subparser $
  command "add" (parseAdd `withInfo` "Add a bookmark") <>
  command "remove" (parseRemove `withInfo` "Remove a bookmark") <>
  command "import" (parseImport `withInfo` "Import from firefox bookmarks")

parseOption :: Parser Option
parseOption =  RootDirectory <$> p
  where
    p = strOption $
        short 'r' <> long "root-dir" <> metavar "DIR" <>
        help "Root directory of bookmark data"

parseRemove :: Parser Command
parseRemove = Remove <$> argument str (metavar "TITLE")

parseAdd :: Parser Command
parseAdd =
  Add <$> argument str (metavar "TITLE")
      <*> argument str (metavar "URL")
      <*> many (argument str (metavar "TAGS"))
  
parseImport :: Parser Command
parseImport =
  ImportFromFF <$> some (argument str (metavar "FILES.."))

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
