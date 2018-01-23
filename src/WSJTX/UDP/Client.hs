----------------------------------------------------------------------------
-- |
-- Module      :  WSJTX.UDP.Server
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Receive UDP packages from WSJTX
module WSJTX.UDP.Server
where

import Control.Monad
import Control.Concurrent

import Control.Exception.Base (bracket)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.EncodeQt (parseUDPPackage)

defaultPort :: PortNumber
defaultPort = 2237

testDump :: IO String
testDump = wsjtxServerBracket defaultPort print (const Prelude.getLine)

wsjtxServerBracket
  :: PortNumber -> (Package -> IO ()) -> (ThreadId -> IO a) -> IO a
wsjtxServerBracket port callback rest
  = bracket (openSocket port) close
  (\sock -> do
     thread <- forkWsjtxServer sock callback
     rest thread
  )


forkWsjtxServer :: Socket -> (Package -> IO ()) -> IO ThreadId
forkWsjtxServer conn callback = forkIO $ forever $ do
  msg <- recv conn 1024
  callback $  parseUDPPackage msg 
  
  
openSocket :: PortNumber -> IO Socket
openSocket udpPort = do
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet udpPort (tupleToHostAddress (127,0,0,1))
  return sock

-- Send a Package to a UDP port.
{-
module WSJTX.UDP.Server
where

import Control.Monad
import Control.Exception.Base (bracket)
import Network.Socket hiding (send)
import Network.Socket.ByteString (send)

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.EncodeQt (packageToUDP)

defaultPort :: PortNumber
defaultPort = 2237
-}

replyWithPackage :: PortNumber -> Package -> IO ()
replyWithPackage udpPort package = bracket
  (do
     sock <- socket AF_INET Datagram defaultProtocol
     connect sock $ SockAddrInet udpPort (tupleToHostAddress (127,0,0,1))
     return sock
   )
   close
   (\sock -> void $ send sock $ packageToUDP package)
