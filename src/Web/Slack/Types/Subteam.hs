{-# LANGUAGE OverloadedStrings #-}

module Web.Slack.Types.Subteam where

import Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import Data.Text (Text)
import Data.Word (Word)
import Web.Slack.Types.Id (SubteamId, TeamId, UserId)

data Subteam =
  Subteam
    { _subteamId :: SubteamId
    , _subteamTeamId :: TeamId
    , _subteamIsUsergroup :: Bool
    , _subteamName :: Text
    , _subteamDescription :: Text
    , _subteamHandle :: Text
    , _subteamIsExternal :: Bool
    , _subteamDateCreate :: Word
    , _subteamDateUpdate :: Word
    , _subteamDateDelete :: Word
    , _subteamAutoType :: Maybe Text
    , _subteamCreatedBy :: UserId
    , _subteamUpdatedBy :: UserId
    , _subteamDeletedBy :: Maybe UserId
    , _subteamPrefs :: Prefs
    , _subteamUsers :: [UserId]
    , _subteamUserCount :: Word -- The Slack API as of 2019-08-16 shows this as a JSON string, but that is not true.
    }
  deriving (Show)

instance FromJSON Subteam where
  parseJSON =
    withObject
      "Subteam"
      (\o ->
         Subteam <$> o .: "id" <*> o .: "team_id" <*> o .: "is_usergroup" <*>
         o .: "name" <*>
         o .: "description" <*>
         o .: "handle" <*>
         o .: "is_external" <*>
         o .: "date_create" <*>
         o .: "date_update" <*>
         o .: "date_delete" <*>
         o .: "auto_type" <*>
         o .: "created_by" <*>
         o .: "updated_by" <*>
         o .: "deleted_by" <*>
         o .: "prefs" <*>
         o .: "users" <*>
         o .: "user_count")

data Prefs =
  Prefs
    { _prefsChannels :: [Text]
    , _prefsGroups :: [Text]
    }
  deriving (Show)

instance FromJSON Prefs where
  parseJSON =
    withObject "Prefs" (\o -> Prefs <$> o .: "channels" <*> o .: "groups")
