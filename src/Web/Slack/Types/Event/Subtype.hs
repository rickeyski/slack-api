{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings #-}
module Web.Slack.Types.Event.Subtype (Subtype(..), subtype) where

import Data.Aeson
import Data.Aeson.Types
import Web.Slack.Types.Time
import Web.Slack.Types.Id
import Web.Slack.Types.File hiding (URL)
import Web.Slack.Types.Comment
import Web.Slack.Types.Bot
import Web.Slack.Types.Item
import Control.Applicative
import Data.Text (Text)

type Username = Text

data Subtype where
  SBotMessage :: BotId -> Maybe Username -> Maybe BotIcons -> Subtype
  SMeMessage  :: Subtype
  SMessageChanged :: MessageUpdate -> Subtype
  SChannelJoin :: Maybe UserId -> Subtype
  SMessageDeleted :: SlackTimeStamp -> Subtype
  SChannelLeave :: Subtype
  SChannelTopic :: Text -> Subtype
  SChannelPurpose :: Text -> Subtype
  SChannelName :: Text -> Text -> Subtype
  SChannelArchive :: [UserId] -> Subtype
  SChannelUnarchive :: Subtype
  SGroupJoin :: Maybe UserId -> Subtype
  SGroupLeave :: Subtype
  SGroupTopic :: Text -> Subtype
  SGroupPurpose :: Text -> Subtype
  SGroupName :: Text -> Text -> Subtype
  SGroupArchive :: [UserId] -> Subtype
  SGroupUnarchive :: Subtype
  SFileShare :: File -> Bool -> Subtype
  SFileComment :: File -> Comment -> Subtype
  SFileMention :: File -> Subtype
  -- These two subtypes are documented as unstable, so they shouldn't
  -- be used.  See https://api.slack.com/events/message/pinned_item
  -- for more detail and updates.
  SPinnedItem :: Subtype
  SUnpinnedItem :: Subtype
  deriving Show

subtype :: String -> Value -> Parser Subtype
subtype s = withObject "subtype" (\v ->
  case s of
    "bot_message" -> SBotMessage <$> v .: "bot_id"  <*> v .:? "username" <*> v .:? "icons"
    "me_message"  -> return SMeMessage
    "message_changed" -> SMessageChanged <$> v .: "message"
    "message_deleted" -> SMessageDeleted <$> v .: "deleted_ts"
    "channel_join"  ->   SChannelJoin  <$> v .:? "inviter"
    "channel_leave"   -> return SChannelLeave
    "channel_topic"   -> SChannelTopic <$> v .: "topic"
    "channel_purpose" -> SChannelPurpose <$> v .: "purpose"
    "channel_name"    -> SChannelName <$> v .: "old_name" <*> v .: "name"
    "channel_archive" -> SChannelArchive <$> v .: "members"
    "channel_unarchive" ->  return SChannelUnarchive

    "group_join"    -> SGroupJoin  <$> v .:? "inviter"
    "group_leave"   -> return SGroupLeave
    "group_topic"   -> SGroupTopic <$> v .: "topic"
    "group_purpose" -> SGroupPurpose <$> v .: "purpose"
    "group_name"    -> SGroupName <$> v .: "old_name" <*> v .: "name"
    "group_archive" -> SGroupArchive <$> v .: "members"
    "group_unarchive" ->  return SGroupUnarchive
    "file_share"    -> SFileShare <$> v .: "file" <*> v .: "upload"
    "file_comment"  -> SFileComment <$> v .: "file" <*> v .: "comment"
    "file_mention"  -> SFileMention <$> v .: "file"
    "pinned_item"   -> pure SPinnedItem
    "unpinned_item" -> pure SUnpinnedItem
    _               -> fail $ "Unrecognised subtype: " ++ s)

