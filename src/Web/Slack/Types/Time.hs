{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Web.Slack.Types.Time where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import Control.Applicative
import Control.Error
import Control.Lens

default ()

-- | A unix timestamp.
newtype Time = Time { _getTime :: Int } deriving (Num, Eq, Ord, Show)

-- | A 'SlackTimeStamp' is a unix timestamp, plus a sequence number that
-- uniquely identifies (e.g.) a message in a channel.
data SlackTimeStamp = SlackTimeStamp { _slackTime :: Time, _timestampUid :: Int } deriving (Show, Ord, Eq)

makeLenses ''SlackTimeStamp
makeLenses ''Time

instance FromJSON SlackTimeStamp where
  parseJSON = withText "SlackTimeStamp"
                (\s -> let (ts, tail -> uid) = break (== '.') (T.unpack s) in
                  SlackTimeStamp
                    <$> parseTimeString ts
                    <*> readZ uid)

instance FromJSON Time where
  parseJSON (Number s) =
    case floatingOrInteger s :: Either Float Int of
      Left _ -> fail "Time.FromJSON: non-integer unix timestamp"
      Right n -> return $ Time n
  parseJSON (String t) = parseTimeString $ T.unpack t
  parseJSON _ = empty

parseTimeString :: String -> Parser Time
parseTimeString s = Time <$> readZ s

instance ToJSON SlackTimeStamp where
  toJSON = String . formatSlackTimeStamp
  toEncoding = text . formatSlackTimeStamp

formatSlackTimeStamp :: SlackTimeStamp -> T.Text
formatSlackTimeStamp sts = T.pack timeString where
  timeString = mconcat [ sts ^. slackTime . getTime . to show
                       , "."
                       , sts ^. timestampUid . to show
                       ]
