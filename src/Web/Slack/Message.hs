{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Web.Slack.Message (sendMessage, sendMessageLater, ping, pingLater) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson          (encode, ToJSON)
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import           Web.Slack.State
import           Web.Slack.Types
import           Data.Time.Clock.POSIX

import Prelude

-- | Returns an action in IO that can be used to send any payload
-- The payload should use the unique provided id
sendLater :: ToJSON a => Slack s ((Int -> a) -> IO ())
sendLater = do
  conn <- use connection
  counter <- use (meta . msgCounter)
  let send payload = do uid <- nextId counter
                        liftIO $ WS.sendTextData conn (encode (payload uid))
  return send

-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: ChannelId -> T.Text -> Slack s ()
sendMessage cid message = do
  send <- sendMessageLater
  liftIO (send cid message)

-- | Returns an IO action that can be used to send a message
sendMessageLater :: Slack s (ChannelId -> T.Text -> IO ())
sendMessageLater = do sender <- sendLater
                      return (\cid message -> sender (\uid -> MessagePayload uid "message" cid message))

-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
ping :: Slack s ()
ping = pingLater >>= liftIO

-- | Returns an IO action that sends a ping packet to the server
pingLater :: Slack s (IO ())
pingLater = do sender <- sendLater
               return $ do now <- round <$> liftIO getPOSIXTime
                           sender (\uid -> PingPayload uid "ping" now)
