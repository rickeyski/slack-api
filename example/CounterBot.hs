{-# LANGUAGE TemplateHaskell, CPP #-}
module CounterBot where

import qualified Data.Text as T (pack)

import Web.Slack
import Web.Slack.Message

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Control.Lens

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken
         }

data CounterState = CounterState
                  { _messageCount :: Int
                  }

makeLenses ''CounterState

-- Count how many messages the bot recieves
counterBot :: SlackBot CounterState
counterBot (Message cid _ _ _ _ _) = do
  num <- userState . messageCount <%= (+1)
  sendMessage cid (T.pack . show $ num)
counterBot _ = return ()

main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
               <$> lookupEnv "SLACK_API_TOKEN"
  runBot (myConfig apiToken) counterBot startState
  where
    startState = CounterState 0
