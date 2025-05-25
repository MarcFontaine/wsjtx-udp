----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.Server
-- Copyright   :  (c) Marc Fontaine 2017-2024
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Receive UDP packages from WSJT-X and reply to UDP packages.
--
-- Reply to WSJT-X:
-- The UDP destination port address to reach the WSJT-X application
-- is the the same address that the WSJT-X uses as source address.
-- (That is not the address that is configured in the GUI).
-- This address is only known after the first package is received from WSJT-X.
-- (WSJT-X regularly sends heard beat packages.)

module WSJTX.UDP.Server
where

import           Control.Concurrent
import           Control.Monad

import           Control.Exception.Base    (bracket)
import           Network.Socket            hiding (openSocket)
import           Network.Socket.ByteString (recvFrom, send)

import           WSJTX.UDP.EncodeQt        (packetToUDP, parseUDPPacket)
import           WSJTX.UDP.NetworkMessage

wsjtxDefaultPort :: PortNumber
wsjtxDefaultPort = 2237

wsjtxDefaultAddr :: HostAddress
wsjtxDefaultAddr = tupleToHostAddress (0,0,0,0)

testDump :: IO ()
testDump = void $ withWsjtxSocket (wsjtxDefaultAddr, wsjtxDefaultPort) $ \sock -> do
  _threadId <- forkIO $ runWsjtxServer sock print
  void getLine

withWsjtxSocket :: (HostAddress, PortNumber) -> (Socket -> IO a) -> IO a
withWsjtxSocket addrPort
  = bracket (openSocket addrPort) close

runWsjtxServer :: Socket -> (PacketWithAddr -> IO ()) -> IO ()
runWsjtxServer sock callback = forever $ do
  (msg, addr) <- recvFrom sock 1024
  callback $ PacketWithAddr (parseUDPPacket msg) addr

openSocket :: (HostAddress, PortNumber) -> IO Socket
openSocket (serverAddr, udpPort) = do
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet udpPort serverAddr
  return sock

replyWithPackets :: (HostAddress, PortNumber) -> [Packet] -> IO ()
replyWithPackets (serverAddr, udpPort) packets = bracket
  (do
     sock <- socket AF_INET Datagram defaultProtocol
     bind sock $ SockAddrInet udpPort serverAddr
-- wait for a package from the client to find out the address
     (_,addr) <- recvFrom sock 1024
     connect sock addr
     return sock
   )
   close
   (\sock -> forM_ packets (send sock . packetToUDP))
