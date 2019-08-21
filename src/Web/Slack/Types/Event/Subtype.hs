{-# LANGUAGE LambdaCase, GADTs, OverloadedStrings, TemplateHaskell, CPP #-}
module Web.Slack.Types.Event.Subtype (Subtype(..), subtype) where

import Data.Aeson
import Data.Aeson.Types
import Web.Slack.Types.Time
import Web.Slack.Types.Id
import Web.Slack.Types.File hiding (URL)
import Web.Slack.Types.Comment
import Web.Slack.Types.Bot
import Web.Slack.Types.Item
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Text (Text)
import Control.Lens.TH

type Username = Text

data Subtype where
  SBotMessage :: BotId -> Maybe Username -> Maybe BotIcons -> Subtype
  SChannelArchive :: [UserId] -> Subtype
  SChannelJoin :: Maybe UserId -> Subtype
  SChannelLeave :: Subtype
  SChannelName :: Text -> Text -> Subtype
  SChannelPurpose :: Text -> Subtype
  SChannelTopic :: Text -> Subtype
  SChannelUnarchive :: Subtype
  SFileComment :: File -> Comment -> Subtype
  SFileMention :: File -> Subtype
  SFileShare :: File -> Bool -> Subtype
  SGroupArchive :: [UserId] -> Subtype
  SGroupJoin :: Maybe UserId -> Subtype
  SGroupLeave :: Subtype
  SGroupName :: Text -> Text -> Subtype
  SGroupPurpose :: Text -> Subtype
  SGroupTopic :: Text -> Subtype
  SGroupUnarchive :: Subtype
  SMeMessage  :: Subtype
  SMessageChanged :: MessageUpdate -> Subtype
  SMessageDeleted :: SlackTimeStamp -> Subtype
  SMessageReplied :: MessageReplied -> Subtype
  SThreadBroadcast :: ThreadBroadcastRoot -> Subtype
  -- These two subtypes are documented as unstable, so they shouldn't
  -- be used.  See https://api.slack.com/events/message/pinned_item
  -- for more detail and updates.
  SPinnedItem :: Subtype
  SUnpinnedItem :: Subtype
  deriving Show

makePrisms ''Subtype

subtype :: String -> Value -> Parser Subtype
subtype s = withObject "subtype" (\v ->
  case s of

    "bot_message" -> SBotMessage <$> v .: "bot_id"  <*> v .:? "username" <*> v .:? "icons"
    "channel_archive" -> SChannelArchive <$> v .: "members"
    "channel_join"  ->   SChannelJoin  <$> v .:? "inviter"
    "channel_leave"   -> return SChannelLeave
    "channel_name"    -> SChannelName <$> v .: "old_name" <*> v .: "name"
    "channel_purpose" -> SChannelPurpose <$> v .: "purpose"
    "channel_topic"   -> SChannelTopic <$> v .: "topic"
    "channel_unarchive" ->  return SChannelUnarchive
    "file_comment"  -> SFileComment <$> v .: "file" <*> v .: "comment"
    "file_mention"  -> SFileMention <$> v .: "file"
    "file_share"    -> SFileShare <$> v .: "file" <*> v .: "upload"
    "group_archive" -> SGroupArchive <$> v .: "members"
    "group_join"    -> SGroupJoin  <$> v .:? "inviter"
    "group_leave"   -> return SGroupLeave
    "group_name"    -> SGroupName <$> v .: "old_name" <*> v .: "name"
    "group_purpose" -> SGroupPurpose <$> v .: "purpose"
    "group_topic"   -> SGroupTopic <$> v .: "topic"
    "group_unarchive" ->  return SGroupUnarchive
    "me_message"  -> return SMeMessage
    "message_changed" -> SMessageChanged <$> v .: "message"
    "message_deleted" -> SMessageDeleted <$> v .: "deleted_ts"
    "message_replied" -> SMessageReplied <$> v .: "message"
    "pinned_item"   -> pure SPinnedItem
    "thread_broadcast" -> SThreadBroadcast <$> v .: "root"
    "unpinned_item" -> pure SUnpinnedItem
    _               -> fail $ "Unrecognised subtype: " ++ s)

