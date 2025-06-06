----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.EncodeQt
-- Copyright   :  (c) Marc Fontaine 2017-2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--  Generic Serializing and deserializng of WSJT-X UDP packages.
--  WSJT-X uses QT the framework and proprietary QT binary
--  format for serialization.
--  Also see NetworkMessage.hpp in WSJTX sources.
--  This module only supports the schema 2 protocol.

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}

module WSJTX.UDP.EncodeQt
where
import           Control.Monad
import           Data.Binary.Get          (ByteOffset, Get, getByteString,
                                           getDoublebe, getInt32be, getWord32be,
                                           getWord64be, getWord8, runGetOrFail)
import           Data.Binary.Put          (Put, putByteString, putDoublebe,
                                           putInt32be, putWord32be, putWord64be,
                                           putWord8, runPut)
import           Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL (ByteString, fromStrict,
                                                  toStrict)
import           Data.Ratio
import           Data.Text                as Text
import           Data.Text.Encoding       as Text
import           Data.Time
import           Data.Word
import           GHC.Generics

import           WSJTX.UDP.NetworkMessage as NetworkMessage

class ToQt a where
  toQt :: a -> Put
  default toQt :: (Generic a, ToQt' (Rep a)) => a -> Put
  toQt x = toQt' (from x)

class ToQt' f where
  toQt' :: f p -> Put

instance ToQt Word8 where toQt = putWord8
instance ToQt Word32 where toQt = putWord32be
instance ToQt Word64 where toQt = putWord64be
instance ToQt Int where toQt = putInt32be . fromIntegral
instance ToQt Double where toQt = putDoublebe

instance ToQt DialFrequency where toQt = putWord64be . unDialFrequency

instance ToQt Bool where
  toQt True  = putWord8 1
  toQt False = putWord8 0

instance ToQt Text where
  toQt txt = do
    let bs = Text.encodeUtf8 txt
    putWord32be $ fromIntegral $ BS.length bs
    putByteString bs

instance ToQt' V1 where toQt' _ = undefined
instance ToQt' U1 where toQt' U1 = return ()
instance (ToQt' f) => ToQt' (M1 i t f) where toQt' (M1 x) = toQt' x
instance (ToQt c) => ToQt' (K1 i c) where toQt' (K1 x) = toQt x
instance (ToQt' f, ToQt' g) => ToQt' (f :*: g)
  where
    toQt' (x :*: y) = toQt' x >> toQt' y

class FromQt' f where
  fromQt' :: Get (f p)

instance FromQt' U1 where fromQt' = return U1
instance (FromQt' f) => FromQt' (M1 i t f) where fromQt' = M1 <$> fromQt'
instance (FromQt c) => FromQt' (K1 i c) where fromQt' = K1 <$> fromQt
instance (FromQt' f, FromQt' g) => FromQt' (f :*: g)
  where
    fromQt' = do
      a <- fromQt'
      b <- fromQt'
      return (a :*: b)

class FromQt a where
  fromQt :: Get a
  default fromQt :: (Generic a, FromQt' (Rep a)) =>  Get a
  fromQt = fmap to fromQt'

instance FromQt Text where
  fromQt = do
    len <- getWord32be
    if len == 0xffffffff then return Text.empty
       else do
         when (len > 1000) $ fail $ "FromQt Text: String length > 1000 : len: " ++ show len
         bs <- getByteString $ fromIntegral len
         return $ Text.decodeUtf8With (\_ _ -> Just '_') bs

instance FromQt Word8 where fromQt = getWord8
instance FromQt Word32 where fromQt = getWord32be
instance FromQt Word64 where fromQt = getWord64be
instance FromQt Int where fromQt = fmap fromIntegral getInt32be
instance FromQt Double where fromQt = getDoublebe
instance FromQt Bool where
  fromQt = do
    f <- getWord8
    if f ==0 then return False else return True

instance FromQt DialFrequency where fromQt = DialFrequency <$> getWord64be

instance FromQt UTCTime where
  fromQt = do
    d <- getWord64be
    t <- fromQt
    _tzone <- getWord8 -- ignored
    return $ UTCTime
      { utctDay = toEnum $ fromIntegral d - 2400000
      , utctDayTime = t
      }

instance ToQt UTCTime where
  toQt t = do
    putWord64be $ fromIntegral (fromEnum $ utctDay t) + 2400000
    toQt $ utctDayTime t
    putWord8 1

-- QTime counts milliseconds
-- https://doc.qt.io/qt-5/qtime.html#details
instance FromQt NominalDiffTime where
  fromQt = do
    t <- getWord32be
    return $ secondsToNominalDiffTime $ fromRational (fromIntegral t % 1000)

instance ToQt NominalDiffTime where
  toQt t = putWord32be $ round (1000 * nominalDiffTimeToSeconds t)

instance FromQt DiffTime where
  fromQt = do
    t <- getWord32be
    return $ picosecondsToDiffTime $ fromIntegral t * 1000000000

instance ToQt DiffTime where
  toQt t = putWord32be $ fromInteger (diffTimeToPicoseconds t `div` 1000000000)

parseUDPPacket :: BS.ByteString -> Packet
parseUDPPacket bs = case parseUDPPacket2 bs of
  Left _x         -> OtherPacket $ BS.unpack bs
  Right (_,_,res) -> res

parseUDPPacket2 ::
     BS.ByteString
  -> Either (BSL.ByteString, ByteOffset, String)
            (BSL.ByteString, ByteOffset, Packet)
parseUDPPacket2 bs = runGetOrFail packet $ BSL.fromStrict bs
  where
    packet :: Get Packet
    packet = do
      qtMagicWord
      schema <- getWord32be
      when (schema /= 2) mzero
      getWord32be >>= \case
          0 -> pc PHeartbeat
          1 -> pc PStatus
          2 -> pc PDecode
          3 -> pc PClear
          4 -> pc PReply
          5 -> pc PQSOLogged
          6 -> pc PClose
          7 -> pc PReplay
          8 -> pc PHaltTx
          9 -> pc PFreeText
          10 -> pc PWSPRDecode
          11 -> pc PLocation
          12 -> pc PLoggedADIF
          13 -> pc PHighlightCallsign
          14 -> pc PSwitchConfiguration
          15 -> pc PConfigure
          _ -> mzero

    pc :: (Generic b1, FromQt' (Rep b1)) => (b1 -> Packet) -> Get Packet
    pc constr = constr . to <$> fromQt'

    qtMagicWord :: Get ()
    qtMagicWord = do
       0xAD <- getWord8
       0xBC <- getWord8
       0xCB <- getWord8
       0xDA <- getWord8
       return ()

packetToUDP :: Packet -> BS.ByteString
packetToUDP p
  = BSL.toStrict $ runPut packet
  where
    packet = case p of
        PHeartbeat x           -> pt 0 x
        PStatus x              -> pt 1 x
        PDecode x              -> pt 2 x
        PClear x               -> pt 3 x
        PReply x               -> pt 4 x
        PQSOLogged x           -> pt 5 x
        PClose x               -> pt 6 x
        PReplay x              -> pt 7 x
        PHaltTx x              -> pt 8 x
        PFreeText x            -> pt 9 x
        PWSPRDecode x          -> pt 10 x
        PLocation x            -> pt 11 x
        PLoggedADIF x          -> pt 12 x
        PHighlightCallsign x   ->  pt 13 x
        PSwitchConfiguration x ->  pt 14 x
        PConfigure x           -> pt 15 x
        OtherPacket l          -> putByteString $ BS.pack l

    pt :: (Generic b1, ToQt' (Rep b1)) => Word32 -> b1 -> Put
    pt tag x = do
      qtMagicWord
      putWord32be 2
      putWord32be tag
      toQt' $ from x

    qtMagicWord :: Put
    qtMagicWord = do
      putWord8 0xAD
      putWord8 0xBC
      putWord8 0xCB
      putWord8 0xDA
