----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Marc Fontaine 2017-2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

import Control.Monad (void)
import Network.Socket
import System.IO (hFlush, stdout)

import Data.Binary.Builder as Binary
import Data.ByteString.Lazy.Char8 as BSL (length, putStr, putStrLn)
import Data.Aeson as Aeson

import WSJTX.UDP.NetworkMessage (PacketWithAddr)
import WSJTX.UDP.Server (runWsjtxServer, withWsjtxSocket)

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
 void $ withWsjtxSocket (wsjtxAddr,wsjtxPort) $ \sock -> do
   void $ runWsjtxServer sock toChrome

toChrome :: PacketWithAddr -> IO ()
toChrome packet = do
  BSL.putStr prefix
  BSL.putStr bs
  hFlush stdout
  where
    prefix = Binary.toLazyByteString $ Binary.putWord32host $ fromIntegral $ BSL.length bs
    bs = Aeson.encode packet
