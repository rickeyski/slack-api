{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, ScopedTypeVariables,
 TemplateHaskell #-}
module Web.Slack.Types.Event  where

import Web.Slack.Types.Channel
import Web.Slack.Types.Bot
import Web.Slack.Types.Base
import Web.Slack.Types.User
import Web.Slack.Types.File
import Web.Slack.Types.IM
import Web.Slack.Types.Id
import Web.Slack.Types.Item
import Web.Slack.Types.Comment
import Web.Slack.Types.Error
import Web.Slack.Types.Event.Subtype
import Web.Slack.Types.Time
import Web.Slack.Types.Presence
import Web.Slack.Types.Subteam

import Data.Aeson
import Data.Aeson.Types

import Control.Lens.TH
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Prelude

type Domain = Text

data Event where
  AccountsChanged :: Event
  BotAdded :: Bot -> Event
  BotChanged :: Bot -> Event
  ChannelArchive :: ChannelId -> UserId -> Event
  ChannelCreated :: Channel -> Event
  ChannelDeleted :: ChannelId -> Event
  ChannelHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  ChannelJoined :: Channel -> Event
  ChannelLeft   :: ChannelId -> Event
  ChannelMarked :: ChannelId -> SlackTimeStamp -> Event
  ChannelRename :: ChannelRenameInfo -> Event
  ChannelUnarchive :: ChannelId -> UserId -> Event
  CommandsChanged :: SlackTimeStamp -> Event
  EmailDomainChange :: Domain -> SlackTimeStamp -> Event
  EmojiChanged :: SlackTimeStamp -> Event
  FileChange  :: FileChangeInfo -> Event
  FileCommentAdded :: File -> Comment -> Event
  FileCommentDeleted :: File -> CommentId -> Event
  FileCommentEdited :: File -> Comment -> Event
  FileCreated :: File -> Event
  FileDeleted :: FileId -> SlackTimeStamp -> Event
  FilePrivate :: FileId -> Event
  FilePublic :: FileReference -> Event
  FileShared :: FileReference -> Event
  FileUnshared :: File -> Event
  GroupArchive :: ChannelId -> Event
  GroupClose :: UserId -> ChannelId -> Event
  GroupHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  GroupJoined :: Channel -> Event
  GroupLeft :: Channel -> Event
  GroupMarked :: ChannelId -> SlackTimeStamp -> Event
  GroupOpen :: UserId -> ChannelId -> Event
  GroupRename :: ChannelRenameInfo -> Event
  GroupUnarchive :: ChannelId -> Event
  Hello :: Event
  HiddenMessage :: ChannelId -> Submitter -> SlackTimeStamp -> Maybe Subtype -> Event
  ImClose :: UserId -> IMId -> Event
  ImCreated :: UserId -> IM -> Event
  ImHistoryChanged :: SlackTimeStamp -> SlackTimeStamp -> SlackTimeStamp -> Event
  ImMarked :: IMId -> SlackTimeStamp -> Event
  ImOpen :: UserId -> IMId -> Event
  ManualPresenceChange :: Presence -> Event
  Message :: ChannelId -> Submitter -> Text -> SlackTimeStamp -> Maybe Subtype -> Maybe Edited -> Event
  MessageError :: Int -> SlackError -> Event
  MessageResponse :: Int -> SlackTimeStamp -> Text -> Event
  Pong :: Time -> Event
  PrefChange :: Pref -> Event
  PresenceChange :: UserId -> Presence -> Event
  ReactionAdded :: UserId -> Text -> UserId {- item author -} -> EmbeddedItem -> SlackTimeStamp -> Event
  ReactionRemoved :: UserId -> Maybe Text -> EmbeddedItem -> SlackTimeStamp -> Event
  ReconnectUrl :: URL -> Event
  StarAdded :: UserId -> Item -> SlackTimeStamp -> Event
  StarRemoved :: UserId -> Item -> SlackTimeStamp -> Event
  StatusChange :: UserId -> Text -> SlackTimeStamp -> Event
  SubteamCreated :: Subteam -> Event
  SubteamUpdated :: Subteam -> Event
  TeamDomainChange :: URL -> Domain -> Event
  TeamJoin :: User -> Event
  TeamMigrationStarted :: Event
  TeamPrefChange :: Pref -> Event
  TeamRenameEvent :: Text -> Event
  UserChange :: User -> Event
  UserTyping :: ChannelId -> UserId -> Event
  -- Unstable
  NoEvent :: Event
  PinAdded :: Event
  PinRemoved :: Event
  -- Parsing failing of an event
  UnknownEvent :: Value -> Event
  deriving (Show)

type Pref = (Text, Value)

instance FromJSON Event where
  parseJSON o@(Object v) = do
    (typ :: Maybe Text) <- v .:? "type"
    case typ of
      Just t -> parseType o t
      Nothing -> do
        (ok :: Bool) <- v .: "ok"
        if ok
          then MessageResponse <$> v .: "reply_to" <*> v .: "ts" <*> v .: "text"
          else MessageError <$> v .: "reply_to" <*> v .: "error"
  parseJSON Null = return NoEvent
  parseJSON _ = error "Expecting object: Event"

parseType :: Value -> Text -> Parser Event
parseType o@(Object v) typ =
    case typ of
      "accounts_changed" -> pure AccountsChanged
      "bot_added" -> BotAdded <$> v .:  "bot"
      "bot_changed" -> BotChanged <$> v .: "bot"
      "channel_archive" -> ChannelArchive <$> v .: "channel" <*> v .: "user"
      "channel_created" -> ChannelCreated <$> v .: "channel"
      "channel_deleted" -> ChannelDeleted <$> v .: "channel"
      "channel_history_changed" -> ChannelHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "channel_joined"  -> ChannelJoined <$> v .: "channel"
      "channel_left"    -> ChannelLeft <$> v .: "channel"
      "channel_marked"  -> ChannelMarked <$> v .: "channel" <*> v .: "ts"
      "channel_rename"  -> ChannelRename <$> v .: "channel"
      "channel_unarchive" -> ChannelUnarchive <$> v .: "channel" <*> v .: "user"
      "commands_changed" -> CommandsChanged <$> v .: "event_ts"
      "email_domain_changed" -> EmailDomainChange <$> v .: "email_domain" <*> v .: "event_ts"
      "emoji_changed" -> EmojiChanged <$> v .: "event_ts"
      "file_change"  -> FileChange <$> parseJSON o
      "file_comment_added" -> FileCommentAdded <$> v .: "file" <*> v .: "comment"
      "file_comment_deleted" -> FileCommentDeleted <$> v .: "file" <*> v .: "comment"
      "file_comment_edited" -> FileCommentEdited <$> v .: "file" <*> v .: "comment"
      "file_created" -> FileCreated <$> v .: "file"
      "file_deleted"  -> FileDeleted <$> v .: "file_id" <*> v .: "event_ts"
      "file_private" -> FilePrivate <$> v .: "file"
      "file_public"  -> FilePublic <$> v .: "file"
      "file_shared"  -> FileShared <$> v .: "file"
      "file_unshared" -> FileUnshared <$> v .: "file"
      "group_archive" -> GroupArchive <$> v .: "channel"
      "group_close" -> GroupClose <$> v .: "user" <*> v .: "channel"
      "group_history_changed" -> GroupHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "group_joined" -> GroupJoined <$> v .: "channel"
      "group_left" ->  GroupLeft <$> v  .: "channel"
      "group_marked" -> GroupMarked <$> v .: "channel" <*> v .: "ts"
      "group_open" ->  GroupOpen <$> v .: "user" <*> v .: "channel"
      "group_rename" -> GroupRename <$> v .: "channel"
      "group_unarchive" -> GroupUnarchive <$> v .: "channel"
      "hello" -> return Hello
      "im_close" -> ImClose <$> v .: "user" <*> v .: "channel"
      "im_created" -> ImCreated <$> v .: "user" <*> v .: "channel"
      "im_history_changed" -> ImHistoryChanged <$> v .: "latest" <*> v .: "ts" <*> v .: "event_ts"
      "im_marked" -> ImMarked <$> v .: "channel" <*> v .: "ts"
      "im_open"     -> ImOpen <$> v .: "user" <*> v .: "channel"
      "manual_presence_change" -> ManualPresenceChange <$> v .: "presence"
      "message" -> do
        subt <- (\case
                  Nothing -> return Nothing
                  Just r  -> Just <$> subtype r o) =<< v .:? "subtype"
        submitter <- case subt of
                      Just (SBotMessage bid _ _) -> return $ BotComment bid
                      _ -> maybe System UserComment <$> v .:? "user"
        void $ (v .: "channel" :: Parser ChannelId)
        hidden <- (\case {Just True -> True; _ -> False}) <$> v .:? "hidden"
        if not hidden
          then Message <$>  v .: "channel" <*> pure submitter  <*> v .: "text" <*> v .: "ts" <*> pure subt <*> v .:? "edited"
          else HiddenMessage <$>  v .: "channel" <*> pure submitter  <*> v .: "ts" <*> pure subt
      "pin_added" -> pure PinAdded
      "pin_removed" -> pure PinRemoved
      "pong" -> Pong <$> v .: "timestamp"
      "pref_change" -> curry PrefChange <$> v .: "name" <*> v .: "value"
      "presence_change" -> PresenceChange <$> v .: "user" <*> v .: "presence"
      "reaction_added" -> ReactionAdded <$> v .: "user" <*> v .: "reaction" <*> v .: "item_user" <*> v .: "item" <*> v .: "event_ts"
      "reaction_removed" -> ReactionRemoved <$> v .: "user" <*> v .:? "name" <*> v .: "item" <*> v .: "event_ts"
      "reconnect_url" -> ReconnectUrl <$> v .: "url"
      "star_added" ->  StarAdded <$> v .: "user" <*> v .: "item" <*> v .: "event_ts"
      "star_removed" -> StarRemoved <$> v .: "user" <*> v .: "item" <*> v .: "event_ts"
      "status_change" -> StatusChange <$> v .: "user" <*> v .: "status" <*> v .: "event_ts"
      "subteam_created" -> SubteamCreated <$> v .: "subteam"
      "subteam_updated" -> SubteamUpdated <$> v .: "subteam"
      "team_domain_change" -> TeamDomainChange <$> v .: "url" <*> v .: "domain"
      "team_join"   -> TeamJoin <$> v .: "user"
      "team_migration_started" -> pure TeamMigrationStarted
      "team_pref_change" -> curry TeamPrefChange <$> v .: "name" <*> v .: "value"
      "team_rename" -> TeamRenameEvent <$> v .: "name"
      "user_change" -> UserChange <$> v .: "user"
      "user_typing" -> UserTyping <$> v .: "channel" <*> v .: "user"
      _ -> return $ UnknownEvent o
parseType _ _ = error "Expecting object"


data Submitter = UserComment UserId | BotComment BotId | System deriving (Show, Eq)

data ChannelRenameInfo = ChannelRenameInfo
                       { _channelRenameId      :: ChannelId
                       , _channelRenameName    :: Text
                       , _channelRenameCreated :: Time } deriving Show

makeLenses ''ChannelRenameInfo

instance FromJSON ChannelRenameInfo where
  parseJSON = withObject "ChannelRenameInfo" (\o -> ChannelRenameInfo <$> o .: "id" <*> o .: "name" <*> o .: "created")


makePrisms ''Event
