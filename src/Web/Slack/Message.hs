{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Web.Slack.Message (sendMessage, makePingPacket, ping) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson          (encode)
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import           Web.Slack.State
import           Web.Slack.Types
import           Data.Time.Clock.POSIX

import Prelude

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

makePingPacket :: Slack s (IO ())
makePingPacket = do
  conn <- use connection
  uid <- counter
  now <- round <$> liftIO getPOSIXTime
  let payload = PingPayload uid "ping" now
  return (ping conn payload)


-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
ping :: WS.Connection -> PingPayload -> IO ()
ping conn payload =
  WS.sendTextData conn (encode payload)


