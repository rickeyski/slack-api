{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Web.Slack.Types.Presence where

import Data.Aeson
import Control.Applicative
import qualified Data.Text as T
import Prelude

data Presence = Away | Active deriving Show

instance FromJSON Presence where
  parseJSON = withText "Presence"
                (\case
                  "active" -> pure Active
                  "away"   -> pure Away
                  s -> fail $ "Unrecognised presence type: " ++ T.unpack s)
