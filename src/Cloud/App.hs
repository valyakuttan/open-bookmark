{-# LANGUAGE FlexibleContexts #-}

----------------------------------------------------------------------
-- |
-- Module : Cloud.App
--
----------------------------------------------------------------------


module Cloud.App
    (
      App
    , Env(..)
    , runApp
    , defaultAppEnvironment

      -- * JSON utilities
    , readJSON
    , writeJSON
    , performIO
    , removeJSON

      -- * Query Environment
    , currentAppEnvironment
    , currentAppConfig
    , currentAppRoot
    , currentAppTime

      -- * File system utilities
    , cloudFilePath
    , bookmarkDirectoryPath
    , tagDirectoryPath
    , createAppDirectory
    , readConsole
    ) where


import           Control.Error
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Char            (toLower)
import           System.Directory

import           Cloud.Config         (Config)
import qualified Cloud.Config         as Cfg
import           Cloud.Types
import           Cloud.Utils


data Env = Env
    { config      :: !Config
    , rootDir     :: !FilePath
    }

newtype App a = App {
    rApp :: StateT POSIXMicroSeconds (ReaderT Env (ExceptT String IO)) a
    }

instance Functor App where
    fmap f (App a) = App $ updateClock >> f <$> a

instance Applicative App where
    pure = App . pure
    App f <*> App a = App $ updateClock >> f <*> a


instance Monad App where
    return = App . return
    App m >>= k = App $ updateClock >> m >>= rApp . k

updateClock :: (MonadState POSIXMicroSeconds m, MonadIO m) => m ()
updateClock = do
    t   <- liftIO currentTimeInPOSIXMicroSeconds
    put t

runApp :: App a -> Env -> IO (Either String a)
runApp app env = do
    t <- currentTimeInPOSIXMicroSeconds
    runExceptT (runReaderT (evalStateT (rApp app) t) env)

-- | Retruns the path where this bookmarkable will be stored
cloudFilePath :: Storable b => b -> App FilePath
cloudFilePath b =
    Cfg.cloudFilePath <$> currentAppConfig <*> currentAppRoot <*> pure b

-- | Returns the directory where bookmarks will be stored.
bookmarkDirectoryPath :: App FilePath
bookmarkDirectoryPath =
    Cfg.bookmarkDirectoryPath <$> currentAppConfig <*> currentAppRoot

-- | Returns the directory where tags will be stored.
tagDirectoryPath :: App FilePath
tagDirectoryPath =
    Cfg.tagDirectoryPath <$> currentAppConfig <*> currentAppRoot

-- | Returns the current 'Config' object
currentAppConfig :: App Config
currentAppConfig = config <$> currentAppEnvironment

-- | Returns the root directory of the cloud home.
currentAppRoot :: App FilePath
currentAppRoot = rootDir <$> currentAppEnvironment


-- | Returns current time.
currentAppTime :: App POSIXMicroSeconds
currentAppTime = App get

currentAppEnvironment :: App Env
currentAppEnvironment = App ask

defaultAppEnvironment :: FilePath -> IO Env
defaultAppEnvironment = return . Env Cfg.defaultConfig

createAppDirectory :: FilePath -> App ()
createAppDirectory path = do
  exists <- performIO $ doesDirectoryExist path
  unless exists $ do
    reply <- readConsole ("Create Directory " ++ path ++ " [Y]es/No : ")
    let ok = toLower (headDef 'y' reply) == 'y'
    when ok $ performIO $ createDirectoryIfMissing True path

readConsole :: String -> App String
readConsole msg = do
  performIO $ putStrLn msg
  performIO getLine

readJSON :: FromJSON a => FilePath -> App (Maybe a)
readJSON path = App $ do
    js <- lift $ lift $ tryIOAction (readJSON' path)
    case js of
        Right a  -> return a
        l -> lift $ lift $ hoistEither l

readJSON' :: FromJSON a => FilePath -> IO (Either String (Maybe a))
readJSON' path = runExceptT action
  where
      action = do
          exits <- tryIOAction (doesFileExist path)
          if exits
              then Just <$> read'
              else return Nothing
      readCloud = tryIOAction . B.readFile
      read' = readCloud path >>= hoistEither . eitherDecode

writeJSON :: ToJSON a => FilePath -> a -> App ()
writeJSON path = performIO . B.writeFile path . encode

removeJSON :: FilePath -> App ()
removeJSON path = performIO $ do
    exists <- doesFileExist path

    when exists $ removeFile path

performIO :: IO a -> App a
performIO = App . lift . lift . tryIOAction

tryIOAction :: IO a -> ExceptT String IO a
tryIOAction = fmapLT show . tryIO
