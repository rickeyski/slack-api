{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.Channel where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)
import Control.Lens.TH

import Web.Slack.Types.Id
import Web.Slack.Types.Time
import Web.Slack.Types.Topic
import {-# SOURCE #-} Web.Slack.Types.ChannelOpt (ChannelOpt)
import Prelude

data Channel = Channel { _channelId         :: ChannelId
                       , _channelName       :: Text
                       , _channelCreated    :: Time
                       , _channelCreator    :: UserId
                       , _channelIsArchived :: Bool
                       , _channelIsGeneral  :: Bool
                       , _channelMembers    :: Maybe [UserId]
                       , _channelTopic      :: Maybe Topic
                       , _channelPurpose    :: Maybe Purpose
                       , _channelIsMember   :: Bool
                       , _channelOpt        :: Maybe ChannelOpt
                       } deriving Show

makeLenses ''Channel

instance FromJSON Channel where
  parseJSON = withObject "Channel" (\o -> Channel <$> o .: "id" <*> o .: "name"
                                                  <*> o .: "created" <*> o .:"creator"
                                                  <*> o .: "is_archived" <*> o .: "is_general"
                                                  <*> o .:? "members" <*> o .:? "topic"
                                                  <*> o .:? "purpose" <*> o .: "is_member"
                                                  <*> (pure $ parseMaybe parseJSON (Object o) :: Parser (Maybe ChannelOpt)))
