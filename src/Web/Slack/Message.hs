{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Web.Slack.Message (sendMessage) where

import           Control.Lens
import           Control.Monad.State
import           Data.Aeson          (encode)
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import           Web.Slack.State
import           Web.Slack.Types

data MessagePayload = MessagePayload
                    { messageId      :: Int
                    , messageType    :: T.Text
                    , messageChannel :: ChannelId
                    , messageText    :: T.Text } deriving Show

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 7} ''MessagePayload)

-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: ChannelId -> T.Text -> Slack s ()
sendMessage cid message = do
  conn <- use connection
  uid  <- counter
  let payload = MessagePayload uid "message" cid message
  slackLog payload
  liftIO $ WS.sendTextData conn (encode payload)

