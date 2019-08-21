{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Web.Slack.Types.File where

import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Control.Lens.TH

import Web.Slack.Types.Id
import Web.Slack.Types.Comment
import Web.Slack.Types.Time
import Web.Slack.Types.Base

import Prelude

newtype FileChangeFile = FileChangeFileInfo { _fileChangeFileInfoFileId :: FileId }  deriving (Show)

instance FromJSON FileChangeFile where
  parseJSON = withObject "FileChangeFile" (\o -> FileChangeFileInfo <$> o .: "id" )

-- | Event information for when a file changes.  Note, the Slack API
-- information for this type, as of 2019-08-16, is incorrect in quite
-- a few ways.
data FileChangeInfo = FileChangeInfo { _fileChangeInfoFileId :: FileId
                             , _fileChangeInfoUserId :: UserId
                             , _fileChangeInfoFile :: FileChangeFile
                             , _fileChangeInfoEventTS :: SlackTimeStamp
                             , _fileChangeInfoTS :: Maybe SlackTimeStamp
                             } deriving (Show)

instance FromJSON FileChangeInfo where
  parseJSON = withObject "FileChangeInfo" (\o -> FileChangeInfo <$> o .: "file_id" <*> o .: "user_id" <*> o .: "file" <*> o .: "event_ts" <*> o .:? "ts")

data File = File { _fileId             :: FileId
                 , _fileTimestamp      :: Time
                 , _fileName           :: Maybe Text
                 , _fileTitle          :: Text
                 , _fileMime           :: Text
                 , _filetype       :: Text
                 , _filePrettyType     :: Text
                 , _fileUser           :: UserId
                 , _fileMode           :: Mode
                 , _fileEditable       :: Bool
                 , _fileIsExternal     :: Bool
                 , _fileExternalType   :: Text
                 , _fileSize           :: Int
                 , _fileUrl            :: FileUrl
                 , _fileThumbs         :: Thumbnail
                 , _filePermalink      :: URL
                 , _fileEditLink       :: Maybe URL
                 , _filePreview        :: Maybe Preview
                 , _filePublic         :: Bool
                 , _filePublicShared   :: Bool
                 , _fileChannels       :: [ChannelId]
                 , _fileGroups         :: [ChannelId]
                 , _fileInitialComment :: Maybe Comment
                 , _fileStars          :: Int
                 , _fileComments       :: Int } deriving Show

data Mode = Hosted | External | Snippet | Post deriving Show

instance FromJSON File where
  parseJSON = withObject "File" (\o ->
              File <$> o .: "id" <*> o .: "timestamp" <*> o .: "name" <*> o .: "title"
                <*> o .: "mimetype" <*> o .: "filetype" <*> o .: "pretty_type" <*> o .: "user"
                <*> o .: "mode" <*> o .: "editable" <*> o .: "is_external" <*> o .: "external_type"
                <*> o .: "size" <*> (parseJSON (Object o) :: Parser FileUrl)
                <*> (parseJSON (Object o) :: Parser Thumbnail)  <*> o .: "permalink" <*> o .:? "edit_link"
                <*>  (pure $ parseMaybe parseJSON (Object o) :: Parser (Maybe Preview))
                <*> o .: "is_public" <*> o .: "public_url_shared"
                <*> o .: "channels" <*> o .: "groups" <*> o .:? "initial_comment"
                <*> fmap (fromMaybe 0) (o .:? "num_stars") <*> o .: "comments_count" )
instance FromJSON FileUrl where
  parseJSON = withObject "FileURL" (\o -> URL <$> o .: "url_private" <*> o .: "url_private_download")
instance FromJSON FileReference where
  parseJSON = withObject "FileReference" (\o -> FileReference <$> o .: "id")
instance FromJSON Thumbnail where
  parseJSON = withObject "Thumbnail" (\o -> Thumbnail <$> o .:? "thumb_64" <*> o .:? "thumb_80" <*>  o .:? "thumb_360" <*> o .:? "thumb_360_gif" <*> o .:? "thumb_360_w" <*> o .:? "thumb_360_h")
instance FromJSON Preview where
  parseJSON = withObject "preview" (\o ->  Preview <$> o .: "preview" <*> o .: "preview_highlight"
                                                   <*> o .: "lines"   <*> o .: "lines_more")

data Preview = Preview { _previewText :: Text, _previewHighlight :: Text, _lines :: Int, _linesMore :: Int } deriving Show
data FileUrl = URL { _private :: Text, _privateDownload :: Text } deriving Show
data FileReference = FileReference { _fileReferenceId :: FileId } deriving Show
data Thumbnail = Thumbnail { _w64 :: Maybe URL, _w80 :: Maybe URL, _w360 :: Maybe URL, _w360gif :: Maybe URL, _width :: Maybe Int, _height :: Maybe Int} deriving Show

makeLenses ''File
makeLenses ''FileUrl
makeLenses ''FileReference
makeLenses ''Thumbnail
makeLenses ''Preview


instance FromJSON Mode where
  parseJSON = withText "mode"
                (\case
                  "hosted"   -> pure Hosted
                  "external" -> pure External
                  "snippet"  -> pure Snippet
                  "post"     -> pure Post
                  s          -> fail $ "Unrecognised mode: " ++ T.unpack s)
