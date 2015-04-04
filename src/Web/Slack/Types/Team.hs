{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.Team where

import Data.Text (Text)
import Web.Slack.Types.Id
import Web.Slack.Types.TeamPreferences
import Web.Slack.Types.Base

import Control.Lens.TH
import Data.Aeson
import Control.Applicative
import Prelude

data Team = Team
          { _teamId                :: TeamId
          , _teamName              :: Text
          , _teamEmailDomain       :: Text
          , _teamDomain            :: Text
          , _teamPreferences       :: TeamPreferences
          , _teamIcons             :: TeamIcons
          , _teamOverStorageLimit  :: Bool
          } deriving Show

data TeamIcons = TeamIcons
               { _teamIcon34      :: URL
               , _teamIcon44      :: URL
               , _teamIcon68      :: URL
               , _teamIcon88      :: URL
               , _teamIcon102     :: URL
               , _teamIcon132     :: URL
               , _teamIconDefault :: Maybe Bool
               } deriving Show

makeLenses ''Team
makeLenses ''TeamIcons

instance FromJSON Team where
  parseJSON = withObject "team"
                (\o -> Team <$> o .: "id" <*> o .: "name"
                        <*> o .: "email_domain" <*> o .: "domain"
                        <*> o .: "prefs"
                        <*> o .: "icon" <*> o .: "over_storage_limit")

instance FromJSON TeamIcons where
 parseJSON = withObject "teamIcons"
              (\o -> TeamIcons
                      <$> o .: "image_34"
                      <*> o .: "image_44"
                      <*> o .: "image_68"
                      <*> o .: "image_88"
                      <*> o .: "image_102"
                      <*> o .: "image_132"
                      <*> o .:? "image_default")
