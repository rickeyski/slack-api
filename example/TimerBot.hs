{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.Environment
import Web.Slack

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf timerBot

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

timerBot :: SlackHandle -> IO ()
timerBot h = forever $ fromMaybeT (return ()) $ do
    Message cid _ msg _ _ _ <- lift $ getNextEvent h
    time <- MaybeT $ pure $ parseTimeMessage msg
    lift $ sendMessage h cid $ "OK, I'll ping you in " <> T.pack (show time) <> "ms"
    void $ lift $ forkIO $ startTimer h time cid

parseTimeMessage :: T.Text -> Maybe Int
parseTimeMessage = fmap (read . T.unpack) . T.stripPrefix "set timer "

startTimer :: SlackHandle -> Int -> ChannelId -> IO ()
startTimer h time cid = do
    threadDelay time
    sendMessage h cid $ T.pack (show time) <> "ms has elapsed!"

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def x = maybe def return =<< runMaybeT x
