{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Web.Slack.Types.Error where

import Data.Aeson
import Control.Lens.TH

data SlackError = SlackError deriving Show

makeLenses ''SlackError

instance FromJSON SlackError where
  parseJSON = withObject "SlackError" (\_ -> return SlackError)
