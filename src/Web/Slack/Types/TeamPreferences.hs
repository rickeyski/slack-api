{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Web.Slack.Types.TeamPreferences where
import           Data.Aeson.TH
import Data.Text (Text)
import           Web.Slack.Utils
import Web.Slack.Types.Id
import Control.Lens.TH


data TeamPreferences = TeamPreferences
                     { _teamDefaultChannels        :: [ChannelId]
                     , _teamMsgEditWindowMins      :: Int
                     , _teamAllowMessageDeletion   :: Bool
                     , _teamHideReferers           :: Bool
                     , _teamDisplayRealNames       :: Bool
                     , _teamWhoCanAtEveryone       :: Text
                     , _teamWhoCanAtChannel        :: Text
                     , _teamWhoCanCreateChannels   :: Text
                     , _teamWhoCanArchiveChannels  :: Text
                     , _teamWhoCanCreateGroups     :: Text
                     , _teamWhoCanPostGeneral      :: Text
                     , _teamWhoCanKickChannels     :: Text
                     , _teamWhoCanKickGroups       :: Text
                     , _teamRetentionType          :: Int
                     , _teamRetentionDuration      :: Int
                     , _teamGroupRetentionType     :: Int
                     , _teamGroupRetentionDuration :: Int
                     , _teamDmRetentionType        :: Int
                     , _teamDmRetentionDuration    :: Int
                     } deriving Show


$(deriveJSON defaultOptions {fieldLabelModifier = toSnake . drop 5} ''TeamPreferences)

--makeLensesWith abbreviatedFields ''TeamPreferences
makeLenses ''TeamPreferences
