{-# LANGUAGE GeneralizedNewtypeDeriving #-}
----------------------------------------------------------------------
-- |
-- Module : App
--
----------------------------------------------------------------------


module App
    (
      App
    , Env(..)
    , runApp
    , getDefaultAppEnvironment
    , currentEnvironment
    , readJSON
    , writeJSON
    , performIO
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           System.Directory

import           Cloud.Config
import           Cloud.Utils


data Env = Env
    { config       :: !Config
    , rootDir      :: !FilePath
    , currentTime  :: !POSIXMicroSeconds
    }

newtype App a = App { rApp :: ReaderT Env (EitherT String IO) a }
              deriving (Functor, Applicative, Monad, Alternative)

runApp :: App a -> Env -> IO (Either String a)
runApp app env = runEitherT (runReaderT (rApp app) env)

currentEnvironment :: App Env
currentEnvironment = App ask

getDefaultAppEnvironment :: FilePath -> IO Env
getDefaultAppEnvironment root = do
      t <- currentTimeInPOSIXMicroSeconds
      return $ Env defaultConfig root t

readJSON :: FromJSON a => FilePath -> App a
readJSON path = App $ do
    js <- lift $ tryIOAction (readJSON' path)
    case js of
        Left msg -> lift (left msg)
        Right a  -> return a

readJSON' :: FromJSON a => FilePath -> IO (Either String a)
readJSON' path = runEitherT action
  where
      action = do
          exits <- tryIOAction (doesFileExist path)
          if exits
              then read'
              else left $ path ++ " doesn't exists.."
      readCloud = tryIOAction . B.readFile
      read' = readCloud path >>= hoistEither . eitherDecode

writeJSON :: ToJSON a => FilePath -> a -> App ()
writeJSON path = performIO . B.writeFile path . encode

performIO :: IO a -> App a
performIO = App . lift . tryIOAction

tryIOAction :: IO a -> EitherT String IO a
tryIOAction = fmapLT show . tryIO
