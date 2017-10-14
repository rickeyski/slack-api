![Travis Build Status](https://travis-ci.org/mpickering/slack-api.svg?branch=master)

Bindings to the Slack RTM API.

These bindings were developed whilst I was interning at [Borders](http://www.borde.rs/).

More information can be found [here](https://api.slack.com/rtm)

Example
=======

``` haskell
module EchoBot where

import System.Environment (lookupEnv)
import Web.Slack

main :: IO ()
main = do
    Just token <- lookupEnv "SLACK_API_TOKEN"
    let config = SlackConfig { _slackApiToken = token }
    withSlackHandle config echoBot

echoBot :: SlackHandle -> IO ()
echoBot h = forever $ do
    event <- getNextEvent h
    case event of
        (Message cid _ msg _ _ _) -> sendMessage h cid msg
        _ -> return ()
```
