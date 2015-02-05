![Travis Build Status](https://travis-ci.org/mpickering/slack-api.svg?branch=master)

Bindings to the Slack RTM API.

These bindings were developed whilst I was interning at [Borders](http://www.borde.rs/).

More information can be found [here](https://api.slack.com/rtm)

Example
=======

``` haskell
module EchoBot where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }

echoBot :: SlackBot ()
echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
echoBot _ = return ()

main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
               <$> lookupEnv "SLACK_API_TOKEN"
  runBot (myConfig apiToken) echoBot ()
```
