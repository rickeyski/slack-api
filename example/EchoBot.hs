module EchoBot where

import Web.Slack
import Web.Slack.Message

myConfig :: SlackConfig
myConfig = SlackConfig
         { slackApiToken = "..." -- Specify your API token here
         }


echoBot :: SlackBot ()
echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
echoBot _ = return ()

main :: IO ()
main = runBot myConfig echoBot ()



