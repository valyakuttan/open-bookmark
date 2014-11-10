{-# LANGUAGE DeriveGeneric #-}

module Utils (
      POSIXMicroSeconds
    , currentTimeInPOSIXMicroSeconds
    , integerToPOSIXMicroSeconds
    , posixMicroSecondsToUTCTime
    , textHash
    , utcTimeToPOSIXMicroSeconds
    ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

newtype POSIXMicroSeconds = POSIXMicroSeconds Integer
                          deriving (Eq, Ord, Generic)

posixMicroSecondsToUTCTime :: POSIXMicroSeconds -> UTCTime
posixMicroSecondsToUTCTime (POSIXMicroSeconds ms)
  = posixSecondsToUTCTime $ fromIntegral t
  where (t,_) = divMod ms (10^6)
  
utcTimeToPOSIXMicroSeconds :: UTCTime -> POSIXMicroSeconds
utcTimeToPOSIXMicroSeconds utc = POSIXMicroSeconds ms
  where ms = round $ utcTimeToPOSIXSeconds utc * (10^6)

integerToPOSIXMicroSeconds :: Integer -> POSIXMicroSeconds
integerToPOSIXMicroSeconds = POSIXMicroSeconds

currentTimeInPOSIXMicroSeconds :: POSIXMicroSeconds
currentTimeInPOSIXMicroSeconds =
  utcTimeToPOSIXMicroSeconds $ unsafePerformIO getCurrentTime

textHash :: Text -> Int
textHash t = 0

instance FromJSON POSIXMicroSeconds
instance ToJSON POSIXMicroSeconds

instance Show POSIXMicroSeconds where
  show = show . posixMicroSecondsToUTCTime
