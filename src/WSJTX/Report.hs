----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.Report
-- Copyright   :  (c) Marc Fontaine 2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

{-# LANGUAGE DeriveGeneric #-}

module WSJTX.Report where

import Data.Fixed
import Data.Text (Text)
import Data.Time
import Data.Word
import GHC.Generics
import Data.Aeson as Aeson

data Report = Report {
   message :: Text
  ,mode    :: Text
  ,band    :: Text
  ,freq    :: Word64
  ,time    :: DiffTime
  ,snr     :: Int
  ,delta_time :: Fixed E2
  ,delta_frequency :: Word32
  ,recv_locator :: Text
  ,recv_callsign :: Text
  ,recv_info    :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Report where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Report
