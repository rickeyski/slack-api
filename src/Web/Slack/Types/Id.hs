{-# LANGUAGE DataKinds, KindSignatures, TemplateHaskell, DeriveGeneric #-}
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
import Data.Hashable
import GHC.Generics

data FieldType = TUser | TBot | TChannel | TFile | TComment | TIM | TTeam deriving (Eq, Show)

newtype Id (a :: FieldType) = Id { _getId :: Text } deriving (Show, Eq, Ord, Generic)


instance ToJSON (Id a) where
  toJSON (Id uid) = String uid

instance FromJSON (Id a) where
  parseJSON = withText "Id" (return . Id)

instance Hashable (Id a)

type UserId    = Id 'TUser
type BotId     = Id 'TBot
type ChannelId = Id 'TChannel
type FileId    = Id 'TFile
type CommentId = Id 'TComment
type IMId      = Id 'TIM
type TeamId    = Id 'TTeam

makeLenses ''Id
