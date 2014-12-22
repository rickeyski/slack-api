{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Slack.Types.Bot where

import Data.Aeson
import Control.Lens.TH
import Control.Applicative
import Data.Text (Text)

import Web.Slack.Types.Id
import Web.Slack.Types.Base


data Bot = Bot
         { _botId    :: BotId
         , _botName  :: Text
         , _botIcons :: BotIcons
         } deriving (Show)


data BotIcons = BotIcons
            { _botIconImage48 :: URL
            } deriving (Show)

makeLenses ''Bot
makeLenses ''BotIcons


instance FromJSON BotIcons where
  parseJSON = withObject "icons" (\v ->
                BotIcons <$> v .: "image_48")

instance FromJSON Bot where
  parseJSON = withObject "bot" (\v ->
                Bot <$> v .: "id" <*> v .: "name" <*> v .: "icons")
