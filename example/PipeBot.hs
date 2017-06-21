{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Maybe
import Pipes
import System.Environment
import Web.Slack

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf pipeBot

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

pipeBot :: SlackHandle -> IO ()
pipeBot h = runEffect $ slackProducer h >-> slackConsumer h

slackProducer :: SlackHandle -> Producer Event IO ()
slackProducer h = forever $ lift (getNextEvent h) >>= yield

slackConsumer :: SlackHandle -> Consumer Event IO ()
slackConsumer h = forever $
    await >>= \case
        (Message cid _ msg _ _ _) -> lift $ sendMessage h cid msg
        _ -> return ()
