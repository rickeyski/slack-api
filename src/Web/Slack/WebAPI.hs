{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.WebAPI
    ( SlackConfig(..)
    , makeSlackCall

      -- * Methods
    , rtm_start
    ) where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Network.Wreq as W
import Web.Slack.Types

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
   { _slackApiToken :: String -- ^ API Token for Bot
   } deriving (Show)

makeLenses ''SlackConfig

makeSlackCall
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> String
    -> (W.Options -> W.Options)
    -> m Value
makeSlackCall conf method setArgs = do
    let url = "https://slack.com/api/" ++ method
    let setToken = W.param "token" .~ [T.pack (conf ^. slackApiToken)]
    let opts = W.defaults & setToken & setArgs
    rawResp <- liftIO $ W.getWith opts url
    resp <- rawResp ^? W.responseBody . _Value ?? "Couldn't parse response"
    case resp ^? key "ok"  . _Bool of
        Just True  -> return resp
        Just False -> throwError $ resp ^. key "error" . _String
        Nothing    -> throwError "Couldn't parse key 'ok' from response"

-------------------------------------------------------------------------------
-- Methods

-- See https://api.slack.com/methods for the docs.

rtm_start
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> m (T.Text, SlackSession)
rtm_start conf = do
    resp <- makeSlackCall conf "rtm.start" id
    url <- resp ^? key "url" . _String ?? "rtm_start: No url!"
    sessionInfo <- fromJSON' resp
    return (url, sessionInfo)

-------------------------------------------------------------------------------
-- Helpers

fromJSON' :: (FromJSON a, MonadError T.Text m) => Value -> m a
fromJSON' x = case fromJSON x of
    Error e -> throwError (T.pack e)
    Success r -> return r

-- | Like '(??)' from Control.Error, but a bit more general and with the
-- right precedence.
infixl 7 ??
(??) :: MonadError e m => Maybe a -> e -> m a
x ?? e = maybe (throwError e) return x

