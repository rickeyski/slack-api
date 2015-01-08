{-# LANGUAGE TemplateHaskell #-}
module Web.Slack.Config (SlackConfig(..), slackApiToken) where

import Control.Lens.TH

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
                 { _slackApiToken :: String -- ^ API Token for Bot
                 } deriving (Show)

makeLenses ''SlackConfig
