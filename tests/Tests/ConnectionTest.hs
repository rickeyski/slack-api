{-# LANGUAGE LambdaCase #-}

module Tests.ConnectionTest (main) where

import Data.Maybe
import System.Environment
import Web.Slack

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf inertBot

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

inertBot :: SlackHandle -> IO ()
inertBot h =
    getNextEvent h >>= \case
        Hello -> return ()
        e -> error ("Unexpected event: " ++ show e)
