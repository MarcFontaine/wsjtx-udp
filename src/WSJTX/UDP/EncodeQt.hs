----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.EncodeQt
-- Copyright   :  (c) Marc Fontaine 2017-2018
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

{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module WSJTX.UDP.EncodeQt
where
import Data.Word
import Data.Text as Text
import Data.Text.Encoding as Text
import Data.Time
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import GHC.Generics
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.Parser.Word8 (word8)
import Control.Monad

import WSJTX.UDP.NetworkMessage as NetworkMessage

class ToQt a where
  toQt :: a -> Put
  default toQt :: (Generic a, ToQt' (Rep a)) => a -> Put
  toQt x = toQt' (from x)
  
class ToQt' f where
  toQt' :: f p -> Put

instance ToQt Word32 where toQt = putWord32be
instance ToQt Int where toQt = putInt32be . fromIntegral
instance ToQt Word64 where toQt = putWord64be
instance ToQt Double where toQt = putDoublebe
instance ToQt Bool where
  toQt True = putWord8 1
  toQt False = putWord8 0

instance ToQt DiffTime where
  toQt t = putWord32be $ fromIntegral (diffTimeToPicoseconds t `div` 1000000000)

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
instance (FromQt' f) => FromQt' (M1 i t f) where fromQt' = fmap M1 $ fromQt'
instance (FromQt c) => FromQt' (K1 i c) where fromQt' = fmap K1 $ fromQt
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
         bs <- getByteString $ fromIntegral len
         return $ Text.decodeUtf8With (\_ _ -> Just '_') bs

instance FromQt Word32 where fromQt = getWord32be
instance FromQt Int where fromQt = fmap fromIntegral  getInt32be
instance FromQt Word64 where fromQt = getWord64be
instance FromQt Double where fromQt = getDoublebe
instance FromQt Bool where
  fromQt = do
    f <- getWord8
    if f ==0 then return False else return True

instance FromQt DiffTime where
  fromQt = do
    t <- getWord32be
    return $ picosecondsToDiffTime $ ((fromIntegral t) * 1000000000)

parseUDPPackage :: BS.ByteString -> Package
parseUDPPackage bs
  = case runGetOrFail package $ BSL.fromStrict bs of
        Left _x -> OtherPackage $ BS.unpack bs
        Right (_,_,res) -> res
  where
    package :: Get Package
    package = do
      qtMagicWord
      schema <- getWord32be
      when (schema /= 2) $ mzero
      getWord32be >>= \case
          0 -> pc PHeartbeat          
          1 -> pc PStatus
          2 -> pc PDecode
          3 -> pc PClear
          4 -> pc PReply
          5 -> pc PLogged
          6 -> pc PClose
          7 -> pc PReplay
          8 -> pc PHaltTx
          9 -> pc PFreeText
          _ -> mzero

    pc :: (Generic b1, FromQt' (Rep b1)) => (b1 -> Package) -> Get Package
    pc constr = fromQt' >>= (return . constr . to)
         
    qtMagicWord :: Get ()
    qtMagicWord = do
       word8 0xAD
       word8 0xBC
       word8 0xCB
       word8 0xDA


packageToUDP :: Package -> BS.ByteString
packageToUDP p
  = BSL.toStrict $ runPut package
  where
    package = case p of
        PHeartbeat x -> pt 0 x
        PStatus x -> pt 1 x
        PDecode x -> pt 2 x        
        PClear x -> pt 3 x
        PReply x -> pt 4 x
        PLogged x -> pt 5 x
        PClose x -> pt 6 x
        PReplay x  -> pt 7 x
        PHaltTx x  -> pt 8 x
        PFreeText x -> pt 9 x
        OtherPackage l -> putByteString $ BS.pack l
           
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
