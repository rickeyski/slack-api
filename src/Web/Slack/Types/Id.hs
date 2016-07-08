{-# LANGUAGE DataKinds, KindSignatures, TemplateHaskell #-}
module Web.Slack.Types.Id
  ( UserId,
    BotId,
    ChannelId,
    FileId,
    CommentId,
    IMId,
    TeamId,
    Id(..),
    getId
  ) where

import Data.Aeson
import Data.Text (Text)
import Control.Lens.TH

data FieldType = TUser | TBot | TChannel | TFile | TComment | TIM | TTeam deriving (Eq, Show)

newtype Id (a :: FieldType) = Id { _getId :: Text } deriving (Show, Eq, Ord)


instance ToJSON (Id a) where
  toJSON (Id uid) = String uid

instance FromJSON (Id a) where
  parseJSON = withText "Id" (return . Id)

type UserId    = Id 'TUser
type BotId     = Id 'TBot
type ChannelId = Id 'TChannel
type FileId    = Id 'TFile
type CommentId = Id 'TComment
type IMId      = Id 'TIM
type TeamId    = Id 'TTeam

makeLenses ''Id
