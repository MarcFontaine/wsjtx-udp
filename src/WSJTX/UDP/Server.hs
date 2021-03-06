----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.Server
-- Copyright   :  (c) Marc Fontaine 2017-2018
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
import Network.Socket hiding (send, recv, recvFrom)
import Network.Socket.ByteString (send, recv , recvFrom)

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.EncodeQt (packageToUDP, parseUDPPackage)

wsjtxDefaultPort :: PortNumber
wsjtxDefaultPort = 2237

testDump :: IO ()
testDump = withWsjtxSocket wsjtxDefaultPort $ \sock -> do
  _threadId <- forkWsjtxServer sock print
  void getLine
  
withWsjtxSocket :: PortNumber -> (Socket -> IO a) -> IO a
withWsjtxSocket port
  = bracket (openSocket port) close
         
forkWsjtxServer :: Socket -> (Package -> IO ()) -> IO ThreadId
forkWsjtxServer conn callback = forkIO $ forever $ do
  msg <- recv conn 1024
  callback $  parseUDPPackage msg 

openSocket :: PortNumber -> IO Socket
openSocket udpPort = do
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet udpPort (tupleToHostAddress (127,0,0,1))
  return sock

replyWithPackages :: PortNumber -> [Package] -> IO ()
replyWithPackages udpPort packages = bracket
  (do
     sock <- socket AF_INET Datagram defaultProtocol
     bind sock $ SockAddrInet udpPort (tupleToHostAddress (127,0,0,1))
-- wait for a package from the client to find out the address
     (_,addr) <- recvFrom sock 1024
     connect sock addr
     return sock
   )
   close
   (\sock -> forM_ packages (send sock . packageToUDP))
