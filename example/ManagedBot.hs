module Main where

import Control.Monad
import Control.Monad.Managed
import Pipes
import Web.Slack

main :: IO ()
main = runManaged $ runEffect $ slackEvents cfg >-> echoConsumer
  where cfg = undefined

slackEvents :: MonadManaged m => SlackConfig -> Producer Event m a
slackEvents cfg = do
    h <- lift $ using $ managed $ withSlackHandle cfg
    forever $ liftIO (getNextEvent h) >>= yield

echoConsumer :: MonadIO m => Consumer Event m a
echoConsumer = for cat (liftIO . print)
