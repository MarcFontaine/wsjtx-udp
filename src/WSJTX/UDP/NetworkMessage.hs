----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.NetworkMessage
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The package types of the WSJT-X UDP protocol.
-- See NetworkMessage.hpp in WSJT-X sources.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module WSJTX.UDP.NetworkMessage
where

import Data.Word
import Data.Text as Text (Text, pack)
import Data.Time
import Data.Aeson as A
import GHC.Generics
import Network.Socket (SockAddr)
--import Lens.Micro.TH

data Heartbeat = Heartbeat {
    heartbeat_client_id :: Text
  , heartbeat_maximum_schema_number :: Word32
  , heartbeat_version      :: Text
  , heartbeat_revision     :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Heartbeat where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Heartbeat where 
  parseJSON = genericParseJSON aesonOptionsDropPrefix

data Status = Status {
    status_client_id :: Text
  , status_dial_frequency :: DialFrequency
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
  , status_watchdog_timeout :: Bool
  , status_submode :: Text
  , status_fast_mode :: Bool
  , status_special_op_mode :: Word8
  , status_frequency_tolerance :: Word32
  , status_tr_period :: Word32
  , status_configuration_name :: Text
  , status_tx_message :: Text
} deriving (Read, Show, Eq, Generic)

instance ToJSON Status where
  toJSON = genericToJSON defaultOptions  
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Status where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
   
data Decode = Decode {
    decode_client_id :: Text
  , decode_new :: Bool
  , decode_time :: NominalDiffTime
  , decode_snr  :: Int
  , decode_delta_time :: Double
  , decode_delta_frequency :: Word32
  , decode_mode :: Text
  , decode_message :: Text
  , decode_low_confidence :: Bool
  , decode_off_air :: Bool
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Decode where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Decode where
  parseJSON = genericParseJSON aesonOptionsDropPrefix

data Clear = Clear {
   clear_client_id :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Clear where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Clear where
  parseJSON = genericParseJSON aesonOptionsDropPrefix

data Reply = Reply {
   reply_client_id :: Text
  ,reply_time :: NominalDiffTime 
  ,reply_reply_snr :: Word32
  ,reply_delta_time :: Double
  ,reply_mode :: Text
  ,reply_message :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Reply where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Reply where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
  
data Logged = Logged {
    logged_client_id :: Text
  , logged_date_time_off :: DateTime
  , logged_dx_call :: Text
  , logged_dx_grid :: Text
  , logged_dial_frequency :: DialFrequency
  , logged_mode :: Text
  , logged_report_send :: Text
  , logged_report_received :: Text
  , logged_tx_power :: Text
  , logged_comments :: Text
  , logged_name :: Text
  , logged_date_time_on :: DateTime
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Logged where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Logged where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
  
data Close = Close {
  close_client_id :: Text
  } deriving (Read, Show, Eq, Generic)

instance ToJSON Close where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Close where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
  
data Replay = Replay {
    replay_client_id :: Text
    }
  deriving (Read, Show, Eq, Generic)

instance ToJSON Replay where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON Replay where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
  
data HaltTx = HaltTx {
    haltTx_client_id :: Text
  , haltTx_auto_tx_only ::  Bool
  } deriving (Read, Show, Eq, Generic)

instance ToJSON HaltTx where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON HaltTx where
  parseJSON = genericParseJSON aesonOptionsDropPrefix
  
data FreeText = FreeText {
    freeText_client_id :: Text
  , freeText_text :: Text
  , freeText_send :: Bool
  } deriving (Read, Show, Eq, Generic)

instance ToJSON FreeText where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding aesonOptionsDropPrefix
instance FromJSON FreeText where
  parseJSON = genericParseJSON aesonOptionsDropPrefix

data Packet
  = PHeartbeat !Heartbeat
  | PStatus !Status
  | PDecode !Decode
  | PClear !Clear
  | PReply !Reply
  | PLogged !Logged
  | PClose !Close
  | PReplay !Replay
  | PHaltTx !HaltTx
  | PFreeText !FreeText
  | OtherPacket [Word8]
  deriving (Read, Show, Eq, Generic)  

instance ToJSON Packet where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Packet where
  parseJSON = genericParseJSON defaultOptions

newtype DialFrequency
  = DialFrequency {unDialFrequency :: Word64}
  deriving (Read, Show, Eq, Generic)

instance ToJSON DialFrequency where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON DialFrequency where
  parseJSON = genericParseJSON defaultOptions

newtype DateTime
  = DateTime {unDateTime :: Word64}
  deriving (Read, Show, Eq, Generic)

instance ToJSON DateTime where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON DateTime where
  parseJSON = genericParseJSON defaultOptions

aesonOptionsDropPrefix :: Options
aesonOptionsDropPrefix
  = defaultOptions {
      fieldLabelModifier = tail . dropWhile (not . (==) '_')
    }

data PacketWithAddr = PacketWithAddr !Packet !SockAddr
  deriving (Show, Eq, Generic)

instance ToJSON PacketWithAddr where
  toJSON (PacketWithAddr p a) = combined
    where
      (Object obj) = toJSON p
      extra = "SockAddr" .= A.String (Text.pack $ show a)
      combined = Object ( obj <> extra)

{-
makeFields ''Heartbeat
makeFields ''Status
makeFields ''Decode
makeFields ''Reply
makeFields ''Logged
makeFields ''HaltTx
makeFields ''FreeText
-}
