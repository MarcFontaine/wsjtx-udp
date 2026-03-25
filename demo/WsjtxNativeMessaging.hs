{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Marc Fontaine 2017-2026
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

import Control.Exception (try, IOException)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import           Network.Socket            hiding (openSocket)
import           Network.Socket.ByteString (recvFrom, send)

import System.Exit (die)
import System.IO (hFlush, hSetBuffering, stdin, stdout, BufferMode(NoBuffering) )

import Data.Binary.Builder as Binary
import Data.Binary.Get as Binary
import Data.ByteString.Lazy.Char8 as BSL (length, putStr, hGet)
import Data.Aeson as Aeson

import           WSJTX.UDP.EncodeQt        (packetToUDP, parseUDPPacket)
import WSJTX.UDP.NetworkMessage (PacketWithAddr(..), AddrReference(..) )
import WSJTX.UDP.Server (withWsjtxSocket)

wsjtxPort :: PortNumber
wsjtxPort = 2237

wsjtxAddr :: HostAddress6
wsjtxAddr = (0, 0, 0, 0)

data Error =
    ReceiveFromMultipleAddresses SockAddr SockAddr
  | ReadPrefix  IOException
  | ReadMessage IOException
  | PacketDecode String
  | SendBeforeReceive
  | BadDestAddress SockAddr AddrReference
  deriving (Show, Eq)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  void $ withWsjtxSocket (wsjtxAddr, wsjtxPort) $ \sock -> do
    exitValue <- newEmptyMVar
    senderAddr <- newMVar (Right Nothing)
    _recvThread <- forkIO $ forever $ receiveUDP senderAddr sock
    _sendThread <- forkIO $ forever $ sendUDP (blockWithError exitValue) senderAddr sock
    err <- takeMVar exitValue
     -- Todo: send Error to Chrome
    die $ show err

receiveUDP :: MVar (Either Error (Maybe SockAddr)) -> Socket -> IO ()
receiveUDP senderAddr sock = forever $ do
  (msg, addr) <- recvFrom sock 1024
  modifyMVar_ senderAddr $ \case
    Right Nothing -> do
      connect sock addr -- Connect to the first sender address
      return $ Right $ Just addr
    Right (Just knownAddr) -> if knownAddr == addr
      then return $ Right (Just addr)
      else return $ Left $ ReceiveFromMultipleAddresses knownAddr addr
    err -> return err
  toChromeNative $ PacketWithAddr (parseUDPPacket msg) (AddrReference $ show addr)

blockWithError :: MVar Error -> Error -> IO ()
blockWithError mvar err = do
  putMVar mvar err
  putMVar mvar err

sendUDP :: (Error -> IO ()) -> MVar (Either Error (Maybe SockAddr)) -> Socket -> IO ()
sendUDP block senderAddr sock = forever $ do
  msg <- fromChromeNative >>= \case
    Left err -> do
      block err
      error "unreachable"
    Right m -> return m
  let (PacketWithAddr packet destAddr) = msg
  knownAddr <- readMVar senderAddr
  case knownAddr of
    Right Nothing -> do
      block SendBeforeReceive
    Right (Just a) -> if (AddrReference $ show a) == destAddr
      then void $ send sock $ packetToUDP packet
      else block $ BadDestAddress a destAddr
    Left err -> block err

toChromeNative :: ToJSON j => j -> IO ()
toChromeNative j = do
  BSL.putStr prefix
  BSL.putStr message
  hFlush stdout
  where
    message = Aeson.encode j
    prefix = Binary.toLazyByteString $ Binary.putWord32host
      $ fromIntegral $ BSL.length message

-- Todo: error Monad
fromChromeNative :: FromJSON j => IO (Either Error j)
fromChromeNative = do
  prefix <- try $ BSL.hGet stdin 4
  case prefix of
    Left err -> return $ Left $ ReadPrefix err
    Right p -> do
      let bodyLen = fromIntegral $ Binary.runGet Binary.getWord32host p
      -- todo: check that bodyLen is within reasonable limits
      r <- try $ BSL.hGet stdin bodyLen
      case r of
        Left err -> return $ Left $ ReadMessage err
        Right bs -> case eitherDecode bs of
          Left err -> return $ Left $ PacketDecode err
          Right x -> return $ Right x
