{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Web.Slack.Types.Item where

import Data.Aeson
import Web.Slack.Types.Id
import Web.Slack.Types.File
import Web.Slack.Types.Comment
import Web.Slack.Types.Time
import Control.Applicative
import Data.Text (Text)
import Web.Slack.Types.Base

import Control.Lens.TH
import Prelude

data Item = MessageItem ChannelId MessageUpdate
          | FileItem File
          | FileCommentItem File Comment
          | ChannelItem ChannelId
          | IMItem ChannelId
          | GroupItem ChannelId
          | EmbeddedMessageItem ChannelId SlackTimeStamp
          | EmbeddedFileItem FileId
          | EmbeddedFileCommentItem FileId CommentId
          deriving Show

instance  FromJSON Item where
  parseJSON = withObject "item" (\o -> do
                (typ :: String) <- o .: "type"
                case typ of
                  "message" -> (MessageItem <$> o .: "channel" <*> o .: "message") <|>
                               (EmbeddedMessageItem <$> o .: "channel" <*> o .: "ts")
                  "file"    -> (FileItem <$> o .: "file") <|>
                               (EmbeddedFileItem <$> o .: "file")
                  "file_comment" -> (FileCommentItem <$> o .: "file" <*> o .: "comment") <|>
                                    (EmbeddedFileCommentItem <$> o .: "file" <*> o .: "file_comment")
                  "channel" -> ChannelItem <$> o .: "channel"
                  "im"      -> IMItem <$> o .: "channel"
                  "group"   -> GroupItem <$> o .: "group"
                  _         -> fail $ "Unrecognised item type: " ++ typ)

data MessageUpdate = MessageUpdate
                   { _messageUpdateUser   :: UserId
                   , _messageUpdateText   :: Text
                   , _messageUpdateTime   :: SlackTimeStamp
                   , _messageUpdateEdited :: Maybe Edited
                   , _messagePermalink    :: Maybe URL
                   } deriving Show


instance FromJSON MessageUpdate where
  parseJSON = withObject "MessageUpdate"
                (\o -> MessageUpdate <$> o .: "user"
                        <*> o .: "text" <*> o .: "ts"
                        <*> o .:? "edited" <*> o .:? "permalink" )

data Edited = Edited { _editedUser :: UserId, _editTimestap :: SlackTimeStamp } deriving Show

makeLenses ''MessageUpdate
makeLenses ''Edited

instance FromJSON Edited where
  parseJSON = withObject "Edited" (\o -> Edited <$> o .: "user" <*> o .: "ts")
