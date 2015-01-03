{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------
-- |
-- Module : BookmarkCloud.Utils
--
----------------------------------------------------------------------


module BookmarkCloud.Utils (
      POSIXMicroSeconds
    , currentTimeInPOSIXMicroSeconds
    , integerToPOSIXMicroSeconds
    , posixMicroSecondsToUTCTime
    , textHash
    , utcTimeToPOSIXMicroSeconds
    ) where


import           Control.Applicative   ((<$>))
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Char             (ord)
import           Data.List             (foldl')
import           Data.Text             (Text, unpack)
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import           GHC.Generics


newtype POSIXMicroSeconds = POSIXMicroSeconds Integer
                          deriving (Eq, Ord, Generic)

posixMicroSecondsToUTCTime :: POSIXMicroSeconds -> UTCTime
posixMicroSecondsToUTCTime (POSIXMicroSeconds ms)
  = posixSecondsToUTCTime $ fromIntegral t
  where (t,_) = divMod ms (10^(6 :: Integer))

utcTimeToPOSIXMicroSeconds :: UTCTime -> POSIXMicroSeconds
utcTimeToPOSIXMicroSeconds utc = POSIXMicroSeconds ms
  where ms = round $ utcTimeToPOSIXSeconds utc * (10^(6 :: Integer))

integerToPOSIXMicroSeconds :: Integer -> POSIXMicroSeconds
integerToPOSIXMicroSeconds = POSIXMicroSeconds

currentTimeInPOSIXMicroSeconds :: IO POSIXMicroSeconds
currentTimeInPOSIXMicroSeconds =
  utcTimeToPOSIXMicroSeconds <$> getCurrentTime

textHash :: Text -> Int
textHash = foldl' churn 29 . unpack
  where churn b c = (b * 31 + ord c) `mod` maxBound

instance FromJSON POSIXMicroSeconds
instance ToJSON POSIXMicroSeconds

instance Show POSIXMicroSeconds where
  show = show . posixMicroSecondsToUTCTime
