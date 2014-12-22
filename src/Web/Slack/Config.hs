module Web.Slack.Config (SlackConfig(..)) where

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
                 { slackApiToken :: String -- ^ API Token for Bot
                 } deriving (Show)
