{-# LANGUAGE TemplateHaskell #-}
module CounterBot where

import qualified Data.Text as T (pack)

import Web.Slack
import Web.Slack.Message

import Control.Lens
import Control.Lens.TH

myConfig :: SlackConfig
myConfig = SlackConfig
         { slackApiToken = "..."
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
main = runBot myConfig counterBot startState
  where
    startState = CounterState 0
