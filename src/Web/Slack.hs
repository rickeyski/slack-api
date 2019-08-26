{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | Bindings to the various Slack APIs, for writing chat bots.
--
-- > main :: IO ()
-- > main = withSlackHandle myConfig echoBot
-- >
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig { _slackApiToken = "your API token here" }
-- >
-- > -- | For all channels of which this bot is a member, it simply watches
-- > -- for messages and echoes them back to the channel they came from.
-- > echoBot :: SlackHandle -> IO ()
-- > echoBot h = forever $ do
-- >     event <- getNextEvent h
-- >     case event of
-- >         Message chan _ msg _ _ _ ->
-- >             sendMessage h chan msg
-- >         _ -> return ()
--
-- Slack exposes a number of APIs which provide different (but overlapping)
-- functionality:
--
-- * The Real-Time Messaging API (<https://api.slack.com/rtm>) is
--   a WebSocket-based API that allows you to receive events from Slack in
--   real time and send basic messages.
-- * The Web API (<https://api.slack.com/web>) consists of HTTP RPC-style
--   methods. It provides support for more complex interactions with Slack,
--   such as posting messages with buttons, uploading files, making
--   reminders, etc.
--
-- This library is mostly about the RTM API. It has some very limited
-- support for using the Web API.
--
-- For more documentationm, take a look at the "example" directory for some
-- working slack bots.
module Web.Slack
    ( -- * Creating slack sessions
      SlackHandle
    , withSlackHandle

      -- * Getting data from slack
    , getNextEvent
    , getSession
    , getConfig

      -- * Sending messages to slack
    , sendMessage
    , sendRichMessage
    , sendPing
    , addReaction

      -- * Type re-exports
    , SlackConfig(..)
    , SlackSession(..)
    , Event(..)
    , module Web.Slack.Types
    ) where

import Control.Error
import Control.Monad.Except
import Data.Aeson
import Data.IORef
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Web.Slack.Types
import Web.Slack.WebAPI
import Wuss
#if MIN_VERSION_base(4,11,0)
-- We get <> from the Prelude < 4.11.0
#else
import Data.Semigroup ((<>))
#endif

-- | This library exposes a simple handle-based API. A `SlackHandle` is
-- a handle to an open RTM session. It can also be used to make calls to
-- the web API.
data SlackHandle = SlackHandle
    { _shConfig     :: SlackConfig
    , _shSession    :: SlackSession
    , _shConnection :: WS.Connection
    , _shCounter    :: IORef Int
    }

-- | Log into slack with the provided credentials and pass the resulting
-- session handle to the callback provided.
withSlackHandle :: SlackConfig -> (SlackHandle -> IO a) -> IO a
withSlackHandle conf fn = crashOnError $ do
    (url, sessionInfo) <- rtm_start conf
    (host, path) <- parseWebSocketUrl url
    liftIO $ runSecureClient host 443 path $ \conn -> do
        freshCounter <- newIORef 1
        let h = SlackHandle
              { _shConfig = conf
              , _shConnection = conn
              , _shSession = sessionInfo
              , _shCounter = freshCounter
              }
        WS.forkPingThread conn 10
        fn h

parseWebSocketUrl :: Monad m => T.Text -> ExceptT T.Text m (String, String)
parseWebSocketUrl url = do
    uri  <- URI.parseURI (T.unpack url) ?? ("Couldn't parse WebSockets URL: " <> url)
    auth <- URI.uriAuthority uri ?? ("No authority: " <> url)
    return (URI.uriRegName auth, URI.uriPath uri)

-- | Retrieve the config used to initiate the session.
getConfig :: SlackHandle -> SlackConfig
getConfig = _shConfig

-- | When the connection is established, the slack server sends a bunch of
-- session information. This is accessible here.
--
-- (Caveat: this information represents things as they were when the
-- session was established; it is liable to become stale. If you care about
-- keeping an up-to-date view of this stuff, you need to track changes to
-- it using 'getNextEvent'.)
getSession :: SlackHandle -> SlackSession
getSession = _shSession

-- | Get the next event in the queue. If the queue is empty, this function
-- blocks until a new event is recieved.
--
-- If you want to write your bot in a streaming style, this function can be
-- easily adapted into a `Producer`:
--
-- > eventProducer :: MonadIO m => SlackHandle -> Producer Event m ()
-- > eventProducer h = forever $ liftIO (getNextEvent h) >>= yield
getNextEvent :: SlackHandle -> IO Event
getNextEvent h@SlackHandle{..} = do
    raw <- WS.receiveData _shConnection
    case eitherDecode raw of
        Left e -> do
            putStrLn $ unlines
                [ show raw
                , e
                , "Please report this failure to the github issue tracker"
                ]
            getNextEvent h
        Right event@(UnknownEvent val) -> do
            putStrLn $ unlines
                [ show val
                , "Failed to parse to a known event"
                , "Please report this failure to the github issue tracker"
                ]
            return event
        Right event ->
            return event

nextMessageId :: SlackHandle -> IO Int
nextMessageId SlackHandle{_shCounter} =
    atomicModifyIORef' _shCounter (\n -> (n+1, n))

-- | Post a simple message to the specified channel. From the slack docs:
--
-- * Clients should limit messages sent to channels to 4000 characters.
-- * Clients should not send more than one message per second sustained.
sendMessage :: SlackHandle -> ChannelId -> T.Text -> IO ()
sendMessage h@SlackHandle{..} cid message = do
    uid <- nextMessageId h
    let payload = MessagePayload uid "message" cid message
    WS.sendTextData _shConnection (encode payload)

-- | Send a ping to the server, which should respond by sending a 'Pong'
-- event.
sendPing :: SlackHandle -> IO ()
sendPing h@SlackHandle{..} = do
    uid <- nextMessageId h
    now <- round <$> getPOSIXTime
    let payload = PingPayload uid "ping" now
    WS.sendTextData _shConnection (encode payload)

-- | Post a complex message using the web API. There's a lot more
-- functionality than is exposed here - see
-- <https://api.slack.com/methods/chat.postMessage>.
--
-- Note that, since this function uses the slack web API (not the RTD api)
-- under the hood, it behaves a bit differently to @sendMessage@. In
-- particular: rich messages sent by your bot will appear as events. You
-- will probably want to explicitly ignore these.
sendRichMessage
    :: SlackHandle -> ChannelId -> T.Text -> [Attachment] -> IO (Either T.Text ())
sendRichMessage h cid msg as =
    runExceptT $ chat_postMessage (getConfig h) cid msg as

-- | Add a reaction to a message in the specified channel.
--
-- Note that, since this function uses the slack web API (not the RTD api)
-- under the hood.
addReaction :: (MonadError T.Text m, MonadIO m)
            => SlackHandle
            -> ChannelId
            -> T.Text
            -> SlackTimeStamp
            -> m ()
addReaction h cid emoji timestamp =
    reactions_add_message (getConfig h) cid emoji timestamp

-------------------------------------------------------------------------------
-- Helpers

crashOnError :: MonadIO m => ExceptT T.Text m a -> m a
crashOnError x = runExceptT x >>= \case
    Left  e -> liftIO $ ioError $ userError (T.unpack e)
    Right a -> return a
