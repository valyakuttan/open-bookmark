----------------------------------------------------------------------
-- |
-- Module : App
--
----------------------------------------------------------------------


module App
    (
      App
    , Env (..)
    , runApp
    , getEnvironment
    , readJSON
    , writeJSON
    , performIO
    ) where


import           Control.Error
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy        as B
import           System.Directory

import           BookmarkCloud.Config
import           BookmarkCloud.Core.Bookmark
import           BookmarkCloud.Core.Tag
import           BookmarkCloud.Utils


data Env = Env
    { config      :: !Config
    , bCloud      :: !BookmarkCloud
    , tCloud      :: !TagCloud
    , rootDir     :: !FilePath
    , currentTime :: !POSIXMicroSeconds
    }

type App = ReaderT Env (EitherT String IO)

runApp :: App a -> Env -> IO (Either String a)
runApp app = runEitherT . runReaderT app

getEnvironment :: FilePath -> IO Env
getEnvironment root = do
      t <- currentTimeInPOSIXMicroSeconds
      let bc = emptyBookmarkCloud t t
          tc = emptyTagCloud t t

      return $ Env defaultConfig bc tc root t

readJSON :: FromJSON a => FilePath -> IO (Either String a)
readJSON path = runEitherT action
  where
      action = do
          exits <- tryIOAction (doesFileExist path)
          if exits
              then readJSON'
              else left $ path ++ " doesn't exists.."
      readCloud = tryIOAction . B.readFile
      readJSON' = readCloud path >>= hoistEither . eitherDecode

writeJSON :: ToJSON a => FilePath -> a -> App ()
writeJSON path = performIO . B.writeFile path . encode

performIO :: IO a -> App a
performIO = lift . tryIOAction

tryIOAction :: IO a -> EitherT String IO a
tryIOAction = fmapLT show . tryIO
