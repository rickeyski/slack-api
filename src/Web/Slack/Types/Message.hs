{-# LANGUAGE TemplateHaskell #-}

module Web.Slack.Types.Message where

import Data.Aeson.TH
import Data.Char
import qualified Data.Text as T
import Web.Slack.Types.Id

data MessagePayload = MessagePayload
    { messageId      :: Int
    , messageType    :: T.Text
    , messageChannel :: ChannelId
    , messageText    :: T.Text
    } deriving (Show)

data PingPayload = PingPayload
    { pingId        :: Int
    , pingType      :: T.Text
    , pingTimestamp :: Int
    } deriving (Show)
$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 7} ''MessagePayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 4} ''PingPayload)
