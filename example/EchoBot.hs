{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Maybe
import System.Environment
import Web.Slack

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf echoBot

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

echoBot :: SlackHandle -> IO ()
echoBot h = forever $ do
    getNextEvent h >>= \case
        (Message cid _ msg _ _ _) -> sendMessage h cid msg
        _ -> return ()
