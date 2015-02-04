module Tests.ConnectionTest (main) where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative
import System.Exit
import System.IO.Unsafe

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }

connectBot :: SlackBot ()
connectBot Hello = unsafePerformIO exitSuccess
connectBot _ = return ()

main :: IO ()
main = do
  apiToken <- fromMaybe (unsafePerformIO exitFailure)
               <$> lookupEnv "SLACK_API_TOKEN"
  runBot (myConfig apiToken) connectBot ()



