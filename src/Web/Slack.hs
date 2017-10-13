{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE CPP                        #-}
------------------------------------------
-- |
-- This module exposes functionality to write bots which responds
-- to `Event`s sent by the RTM API. By using the user state parameter `s`
-- complicated interactions can be established.
--
-- This basic example echos every message the bot recieves.
-- Other examples can be found in the
-- @<https://github.com/mpickering/slack-api/tree/master/example example>@ directory.
--
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig
-- >         { _slackApiToken = "..." -- Specify your API token here
-- >         }
-- >
-- > -- type SlackBot s = Event -> Slack s ()
-- > echoBot :: SlackBot ()
-- > echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echoBot _ = return ()
-- >
-- > main :: IO ()
-- > main = runBot myConfig echoBot ()
--
module Web.Slack ( runBot
                 -- Re-exports
                 , Slack(..)
                 , SlackBot
                 , SlackState(..)
                 , userState
                 , session
                 , module Web.Slack.Types
                 , SlackConfig(..)
                 ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Lens
import           Control.Monad.Except
import qualified Control.Monad.State        as S
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as T
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS

import           Data.Aeson

import           Web.Slack.State
import           Web.Slack.Types
import           Web.Slack.WebAPI
import           Wuss

-- | Run a `SlackBot`. The supplied bot will respond to all events sent by
-- the Slack RTM API.
--
-- Be warned that this function will throw an `IOError` if the connection
-- to the Slack API fails.
runBot :: forall s . SlackConfig -> SlackBot s -> s -> IO ()
runBot conf bot start = do
  (url, sessionInfo) <- crashOnError $ rtm_start conf
  let partialState :: Metainfo -> SlackState s
      partialState metainfo = SlackState metainfo sessionInfo start conf
  putStrLn "rtm.start call successful"
  case parseWebSocketUrl (T.unpack url) of
    Just (host, path) ->
      runSecureClient host port path (mkBot partialState bot)
    Nothing -> error $ "Couldn't parse WebSockets URL: " ++ T.unpack url
  where
    port = 443
    parseWebSocketUrl :: String -> Maybe (String, String)
    parseWebSocketUrl url = do
      uri  <- URI.parseURI url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)
    crashOnError :: ExceptT T.Text IO a -> IO a
    crashOnError = either (ioError . userError . T.unpack) return <=< runExceptT

mkBot :: (Metainfo -> SlackState s) -> SlackBot s -> WS.ClientApp ()
mkBot partialState bot conn = do
    let initMeta = Meta conn 0
    WS.forkPingThread conn 10
    botLoop (partialState initMeta) bot

botLoop :: forall s . SlackState s -> SlackBot s -> IO ()
botLoop st f =
  () <$ (flip S.runStateT st  . runSlack $ forever loop)
  where
    loop :: Slack s ()
    loop = do
      conn <- use connection
      raw <- liftIO $ WS.receiveData conn
      let (msg :: Either String Event) = eitherDecode raw
      case msg of
        Left e -> do
                    liftIO $ BC.putStrLn raw
                    liftIO $ putStrLn e
                    liftIO . putStrLn $ "Please report this failure to the github issue tracker"
        Right event@(UnknownEvent e) -> do
                    liftIO . print $ e
                    liftIO . putStrLn $ "Failed to parse to a known event"
                    liftIO . putStrLn $ "Please report this failure to the github issue tracker"
                    -- Still handle the event if a user wants to
                    f event
        Right event -> f event

