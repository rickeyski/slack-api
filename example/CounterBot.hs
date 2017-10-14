{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Text as T
import System.Environment
import Web.Slack

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

data CounterState = CounterState
                  { _messageCount :: Int
                  }

makeLenses ''CounterState

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf $ \h -> evalStateT (counterBot h) (CounterState 0)

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

-- Count how many messages the bot recieves
counterBot :: SlackHandle -> StateT CounterState IO ()
counterBot h = forever $
    liftIO (getNextEvent h) >>= \case
        Message cid _ _ _ _ _ -> do
            num <- messageCount <%= (+1)
            liftIO $ sendMessage h cid (T.pack . show $ num)
        _ -> return ()
