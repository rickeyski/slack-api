{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.Comment where

import Data.Aeson
import Control.Applicative
import Control.Lens.TH
import Data.Text (Text)

import Web.Slack.Types.Id
import Web.Slack.Types.Time
import Prelude

data Comment = Comment
             { _commentId        :: CommentId
             , _commentTimestamp :: Time
             , _commentUser      :: UserId
             , _commentComment   :: Text
             } deriving (Show)

makeLenses ''Comment

instance FromJSON Comment where
  parseJSON = withObject "comment" (\o ->
                Comment <$> o .: "id" <*> o .: "timestamp" <*> o .: "user" <*> o .: "comment")
