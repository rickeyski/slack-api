{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.Channel where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)
import Control.Lens.TH
import Data.Maybe (fromMaybe)

import Web.Slack.Types.Id
import Web.Slack.Types.Time
import Web.Slack.Types.Topic
import {-# SOURCE #-} Web.Slack.Types.ChannelOpt (ChannelOpt)
import Prelude

data Channel = Channel { _channelId         :: ChannelId
                       , _channelName       :: Text
                       , _channelCreated    :: Time
                       , _channelCreator    :: UserId
                       , _channelIsArchived :: Maybe Bool
                       , _channelIsGeneral  :: Bool
                       , _channelMembers    :: Maybe [UserId]
                       , _channelTopic      :: Maybe Topic
                       , _channelPurpose    :: Maybe Purpose
                       , _channelIsMember   :: Bool
                       , _channelOpt        :: Maybe ChannelOpt
                       , _channelIsGroup    :: Bool
                       } deriving Show

makeLenses ''Channel

defaultToFalse :: Object -> Text -> Parser Bool
defaultToFalse o tag = fmap (fromMaybe False) $ o .:? tag

instance FromJSON Channel where
  parseJSON = withObject "Channel" (\o -> Channel <$> o .: "id" <*> o .: "name"
                                                  <*> o .: "created" <*> o .:"creator"
                                                  <*> o .:? "is_archived" <*> defaultToFalse o "is_general"
                                                  <*> o .:? "members" <*> o .:? "topic"
                                                  <*> o .:? "purpose" <*> defaultToFalse o "is_member"
                                                  <*> (pure $ parseMaybe parseJSON (Object o) :: Parser (Maybe ChannelOpt))
                                                  <*> o .:? "is_group" .!= False)
