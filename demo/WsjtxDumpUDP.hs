----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Marc Fontaine 2017-2024
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

import Control.Monad (void,forever)
import Control.Concurrent(threadDelay)
import Network.Socket

import Data.ByteString.Lazy.Char8 as BSL (putStrLn)
import Data.Aeson as Aeson

import WSJTX.UDP.Server (runWsjtxServer, withWsjtxSocket)

wsjtxPort :: PortNumber
wsjtxPort = 2237

wsjtxAddr :: HostAddress
wsjtxAddr = tupleToHostAddress (0,0,0,0)

main :: IO ()
main = do
 void $ withWsjtxSocket (wsjtxAddr,wsjtxPort) $ \sock -> do
   void $ runWsjtxServer sock (BSL.putStrLn . Aeson.encode)
