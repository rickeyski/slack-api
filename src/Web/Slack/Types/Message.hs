{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Slack.Types.Message where

import Data.Aeson
import Data.Aeson.TH
import Data.Default.Class
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import GHC.Generics
import Web.Slack.Types.Base
import Web.Slack.Types.Id
import Web.Slack.Utils

data MessagePayload = MessagePayload
    { messageId      :: Int
    , messageType    :: T.Text
    , messageChannel :: ChannelId
    , messageText    :: T.Text
    } deriving (Show)

data PingPayload = PingPayload
    { pingId        :: Int
    , pingType      :: T.Text
    , pingTimestamp :: Int
    } deriving (Show)

data Attachment = Attachment
    { attachmentFallback :: T.Text
        -- ^ A plain-text summary of the attachment.
    , attachmentColor :: AttachmentColor
        -- ^ Used to color the border along the left side of the message
        -- attachment.
    , attachmentPretext :: Maybe T.Text
        -- ^ Optional text that appears above the message attachment block.
    , attachmentAuthorName :: Maybe T.Text
        -- ^ Small text used to display the author's name.
    , attachmentAuthorLink :: Maybe URL
        -- ^ A valid URL that will hyperlink the author_name text mentioned
        -- above.
    , attachmentAuthorIcon :: Maybe URL
        -- ^ A valid URL that displays a small 16x16px image to the left of
        -- the author_name text.
    , attachmentTitle :: Maybe T.Text
        -- ^ The title is displayed as larger, bold text near the top of
        -- a message attachment.
    , attachmentTitleLink :: Maybe URL
        -- ^ By passing a valid URL, the title text will be hyperlinked.
    , attachmentText :: Maybe T.Text
        -- ^ This is the main text in a message attachment, and can contain
        -- standard message markup.
    , attachmentFields :: [Field]
    , attachmentImageUrl :: Maybe URL
        -- ^ An image file that will be displayed inside a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 400x500px.
    , attachmentThumbUrl :: Maybe URL
        -- ^ Displayed as a thumbnail on the right side of a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 75x75px.
    , attachmentFooter :: Maybe T.Text
        -- ^ Add some brief text to help contextualize and identify an
        -- attachment.
    , attachmentFooterIcon :: Maybe URL
        -- ^ Render a small icon beside your footer text. Scaled to 16x16px.
    , attachmentTs :: Maybe POSIXTime
        -- ^ Display an additional timestamp value as part of the
        -- attachment's footer.
    }

data Field = Field
    { fieldTitle :: Maybe T.Text
        -- ^ Shown as a bold heading above the value text. It cannot
        -- contain markup and will be escaped for you.
    , fieldValue :: T.Text
        -- ^ The text value of the field. It may contain standard message
        -- markup and must be escaped as normal. May be multi-line.
    , fieldShort :: Bool
        -- ^ Whether the value is short enough to be displayed side-by-side
        -- with other values.
    }

data AttachmentColor
    = DefaultColor       -- grey
    | GoodColor          -- green
    | WarningColor       -- yellow
    | DangerColor        -- red
    | CustomColor T.Text -- hexadecimal RGB colour, eg. CustomColor "#439FE0"
    deriving (Generic)

instance Default Attachment where
    def = Attachment
        { attachmentFallback = ""
        , attachmentColor = DefaultColor
        , attachmentPretext = Nothing
        , attachmentAuthorName = Nothing
        , attachmentAuthorLink = Nothing
        , attachmentAuthorIcon = Nothing
        , attachmentTitle = Nothing
        , attachmentTitleLink = Nothing
        , attachmentText = Nothing
        , attachmentFields = []
        , attachmentImageUrl = Nothing
        , attachmentThumbUrl = Nothing
        , attachmentFooter = Nothing
        , attachmentFooterIcon = Nothing
        , attachmentTs = Nothing
        }

instance ToJSON AttachmentColor where
    toEncoding x = toEncoding $ case x of
        DefaultColor  -> Nothing
        GoodColor     -> Just "good"
        WarningColor  -> Just "warning"
        DangerColor   -> Just "danger"
        CustomColor c -> Just c

$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 7}  ''MessagePayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 4}  ''PingPayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 10} ''Attachment)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 5}  ''Field)
