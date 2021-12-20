----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.Server
-- Copyright   :  (c) Marc Fontaine 2017-2020
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Receive UDP packages from WSJT-X and reply to UDP packages.
-- The UDP destination port address to reach the WSJT-X application
-- is the the same address that the WSJT-X uses as source address.
-- (That not the address that is configured in the GUI).
-- This address is only known after the first package is received from WSJT-X.
-- (WSJT-X regularly sends heard beat packages.)

module WSJTX.UDP.Server
where

import Control.Monad
import Control.Concurrent

import Control.Exception.Base (bracket)
import Network.Socket
import Network.Socket.ByteString (send, recv , recvFrom)

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.EncodeQt (packetToUDP, parseUDPPacket)

wsjtxDefaultPort :: PortNumber
wsjtxDefaultPort = 2237

wsjtxDefaultAddr :: HostAddress
wsjtxDefaultAddr = tupleToHostAddress (0,0,0,0)

testDump :: IO ()
testDump = withWsjtxSocket (wsjtxDefaultAddr, wsjtxDefaultPort) $ \sock -> do
  _threadId <- forkWsjtxServer sock print
  void getLine

withWsjtxSocket :: (HostAddress, PortNumber) -> (Socket -> IO a) -> IO a
withWsjtxSocket addrPort
  = bracket (openSocket addrPort) close
         
forkWsjtxServer :: Socket -> (Packet -> IO ()) -> IO ThreadId
forkWsjtxServer conn callback = forkIO $ forever $ do
  msg <- recv conn 1024
  callback $  parseUDPPacket msg

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
