{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.Topic where

import Data.Aeson
import Data.Text (Text)
import Control.Applicative
import Control.Lens.TH
import Prelude

type Purpose = Topic

data Topic = Topic
           { _topicValue   :: Text
           , _topicCreator :: Text
           , _topicLastSet :: Int
           } deriving (Show)

makeLenses ''Topic

instance FromJSON Topic where
  parseJSON = withObject "topic" (\o ->
                Topic <$> o .: "value" <*> o .: "creator" <*> o .: "last_set")
