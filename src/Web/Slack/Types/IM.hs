{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.IM where

import Data.Aeson
import Data.Aeson.Types
import Data.Coerce
import Control.Applicative
import Control.Lens.TH

import Web.Slack.Types.Id
import Web.Slack.Types.Time
import {-# SOURCE #-} Web.Slack.Types.ChannelOpt (ChannelOpt)

import Prelude

data IM = IM
        { _imId      :: IMId
        , _imUser    :: UserId
        , _imCreated :: Time
        , _imIsOpen  :: Bool
        , _imIsIm    :: Bool
        , _imOpt     :: Maybe ChannelOpt
        } deriving (Show)

makeLenses ''IM

instance FromJSON IM where
  parseJSON = withObject "IM" (\o ->
               IM <$> o .: "id" <*> o .: "user"
                <*> o .: "created"
                <*> o .: "is_open"
                <*> o .: "is_im"
                <*> (pure $ parseMaybe parseJSON (Object o) :: Parser (Maybe ChannelOpt)))

imToChannel :: IMId -> ChannelId
imToChannel = coerce

channelToIM :: ChannelId -> IMId
channelToIM = coerce
