module Web.Slack.Types.ChannelOpt (ChannelOpt) where

import Data.Aeson

data ChannelOpt

instance FromJSON ChannelOpt

instance Show ChannelOpt
