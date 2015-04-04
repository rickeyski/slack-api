{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.ChannelOpt where

import Data.Aeson
import Control.Lens.TH
import Control.Applicative
import Web.Slack.Types.Event
import Web.Slack.Types.Time


import Prelude

data ChannelOpt = ChannelOpt
                { _channelOptLastRead    :: SlackTimeStamp
                , _channelOptUnreadCount :: Int
                , _channelOptLatest      :: Event
                } deriving (Show)

makeLenses ''ChannelOpt

instance FromJSON ChannelOpt where
  parseJSON = withObject "ChannelOpt"
                (\o -> ChannelOpt
                        <$> o .: "last_read"
                        <*> o .: "unread_count"
                        <*> o .: "latest")
