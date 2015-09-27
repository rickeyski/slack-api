{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Web.Slack.Types.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Control.Lens.TH

import Web.Slack.Types.Id
import Web.Slack.Types.Base

import Prelude


data User = User
          { _userId         :: UserId
          , _userName       :: Text
          , _userDeleted    :: Bool
          , _userColor      :: Text
          , _userProfile    :: Profile
          , _userPermission :: Permissions
          , _userHasFiles   :: Bool
          , _userTimezone   :: Timezone
          } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "User" (\o -> User <$> o .: "id" <*> o .: "name" <*> o .: "deleted"
                                        <*> (o .:? "color" .!= "000000")
                                        <*> o .: "profile" <*> (parseJSON (Object o) :: Parser Permissions)
                                        <*> fmap (fromMaybe False) (o .:? "has_files")
                                        <*> ((return $ fromMaybe defaultTimezone (parseMaybe parseJSON (Object o))) :: Parser Timezone))

defaultTimezone :: Timezone
defaultTimezone = Timezone Nothing "Pacific Standard Time" (-28800)


instance FromJSON Permissions where
  parseJSON = withObject "Permissions" (\o -> let v x = (o .:? x .!= False) in
                                        Permissions <$> v "is_admin" <*> v "is_owner"
                                          <*> v "is_primary_owner" <*> v "is_restricted"
                                          <*> v "is_ultra_restricted" <*> v "is_bot")

data Timezone = Timezone
              { _timezoneDesc   :: Maybe Text
              , _timezoneLabel  :: Text
              , _timezoneOffset :: Int
              } deriving Show

instance FromJSON Timezone where
  parseJSON = withObject "timezone" (\o -> Timezone <$> o .:? "tz" <*> o .: "tz_label" <*> o .: "tz_offset")

data Permissions = Permissions
                 { _isAdmin           :: Bool
                 , _isOwner           :: Bool
                 , _isPrimaryOwner    :: Bool
                 , _isRestricted      :: Bool
                 , _isUltraRestricted :: Bool
                 , _isBot             :: Bool
                 } deriving (Show)

data Profile = Profile
             { _profileFirstName          :: Maybe Text
             , _profileLastName           :: Maybe Text
             , _profileRealName           :: Maybe Text
             , _profileRealNameNormalized :: Maybe Text
             , _profileTitle              :: Maybe Text
             , _progileEmail              :: Maybe Text
             , _profileSkype              :: Maybe Text
             , _profilePhone              :: Maybe Text
             , _profileImage24            :: URL
             , _profileImage32            :: URL
             , _profileImage48            :: URL
             , _profileImage72            :: URL
             , _profileImage192           :: URL
             } deriving (Show)

makeLenses ''Profile
makeLenses ''Permissions
makeLenses ''Timezone
makeLenses ''User

instance FromJSON Profile where
  parseJSON = withObject "Profile"
                (\o -> let v = (o .:)
                           vm = (o .:?) in
                  Profile <$> vm "first_name" <*> vm "last_name" <*> vm "real_name"
                          <*> vm "real_name_normalized" <*> vm "title" <*> vm "email"
                          <*> vm "skype" <*> vm "phone" <*> v "image_24" <*> v "image_32"
                          <*> v "image_48" <*> v "image_72" <*> v "image_192")

type Username = Text
