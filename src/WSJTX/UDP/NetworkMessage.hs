----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.NetworkMessage
-- Copyright   :  (c) Marc Fontaine 2017-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The package types of the WSJT-X UDP protocol.
-- See NetworkMessage.hpp in WSJT-X sources.

{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module WSJTX.UDP.NetworkMessage
where

import Data.Fixed
import Data.Word
import Data.Text as Text (Text)
import Data.Time
import Data.Aeson
import GHC.Generics
--import Lens.Micro.TH

data Heartbeat = Heartbeat {
    heartbeat_client_id :: Text
  , heartbeat_maximum_schema_number :: Word32
  , heartbeat_version      :: Text
  , heartbeat_revision     :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Heartbeat where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Heartbeat where 
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix

data Status = Status {
    status_client_id :: Text
  , status_dial_frequency :: Word64
  , status_mode :: Text
  , status_dx_call :: Text
  , status_report :: Text
  , status_tx_mode :: Text
  , status_tx_enabled :: Bool
  , status_transmitting :: Bool
  , status_decoding :: Bool
  , status_rx_df :: Word32
  , status_tx_df :: Word32
  , status_de_call :: Text
  , status_de_grid :: Text
  , status_dx_grid :: Text
  , status_tx_watchdog :: Bool
  , status_submode :: Text
  , status_fast_mode :: Bool
} deriving (Read, Show, Eq, Generic)

instance ToJSON Status where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Status where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
   
data Decode = Decode {
    decode_client_id :: Text
  , decode_new :: Bool
  , decode_time :: DiffTime
  , decode_snr  :: Int
  , decode_delta_time :: Double
  , decode_delta_frequency :: Word32
  , decode_mode :: Text
  , decode_message :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Decode where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Decode where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix

data Clear = Clear {
   clear_client_id :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Clear where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Clear where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix

data Reply = Reply {
   reply_client_id :: Text
  ,reply_time :: DiffTime 
  ,reply_reply_snr :: Word32
  ,reply_delta_time :: Double
  ,reply_mode :: Text
  ,reply_message :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Reply where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Reply where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
  
data Logged = Logged {
    logged_client_id :: Text
  , logged_date_time_off :: Word64
  , logged_dx_call :: Text
  , logged_dx_grid :: Text
  , logged_dial_frequency :: Word64
  , logged_mode :: Text
  , logged_report_send :: Text
  , logged_report_received :: Text
  , logged_tx_power :: Text
  , logged_comments :: Text
  , logged_name :: Text
  , logged_date_time_on :: Word64
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Logged where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Logged where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
  
data Close = Close {
  close_client_id :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Close where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Close where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
  
data Replay = Replay {
    replay_client_id :: Text
    }
  deriving (Read, Show, Eq, Generic)

instance ToJSON Replay where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON Replay where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
  
data HaltTx = HaltTx {
    haltTx_client_id :: Text
  , haltTx_auto_tx_only ::  Bool
  } deriving (Read, Show, Eq, Generic)

instance ToJSON HaltTx where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON HaltTx where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix
  
data FreeText = FreeText {
    freeText_client_id :: Text
  , freeText_text :: Text
  , freeText_send :: Bool
  } deriving (Read, Show, Eq, Generic)

instance ToJSON FreeText where
  toEncoding = genericToEncoding $ aesonOptionsDropPrefix
instance FromJSON FreeText where
  parseJSON = genericParseJSON $ aesonOptionsDropPrefix

data Package
  = PHeartbeat Heartbeat
  | PStatus Status
  | PDecode Decode
  | PClear Clear
  | PReply Reply
  | PLogged Logged
  | PClose Close
  | PReplay Replay
  | PHaltTx HaltTx
  | PFreeText FreeText
  | OtherPackage [Word8] 
  deriving (Read, Show, Eq, Generic)  

instance ToJSON Package where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions
  
instance Read DiffTime where
  readsPrec p input
     = [(picosecondsToDiffTime $ floor  (f*1000000000000)
        ,Prelude.tail r)]
    where
      f :: Pico
      r :: String
      (f,r) = Prelude.head $ readsPrec p input

--  toEncoding = genericToEncoding defaultOptions

aesonOptionsDropPrefix :: Options
aesonOptionsDropPrefix
  = defaultOptions {
      fieldLabelModifier = tail . snd . break ((==) '_')
    }

{-
makeFields ''Heartbeat
makeFields ''Status
makeFields ''Decode
makeFields ''Reply
makeFields ''Logged
makeFields ''HaltTx
makeFields ''FreeText
-}
