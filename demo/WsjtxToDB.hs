----------------------------------------------------------------------------
-- |
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Testapplication
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (void,forever)
import Control.Concurrent(threadDelay)
import Network.Socket

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.Server (forkWsjtxServer, withWsjtxSocket)

wsjtxPort :: PortNumber
wsjtxPort = 2237

wsjtxAddr :: HostAddress
wsjtxAddr = tupleToHostAddress (0,0,0,0)

main :: IO ()
main = do
 conn <- connectPostgreSQL "" -- Reads from ENV: PGHOST,PGUSER,PGPASSWORD
 withWsjtxSocket (wsjtxAddr,wsjtxPort) $ \sock -> do
   void $ forkWsjtxServer sock (dbWrite conn)
   forever $ threadDelay 100000000
 return ()

dbWrite :: Connection -> Packet -> IO ()
dbWrite conn packet = case packet of
  PDecode p -> do
--    print p
    void $ execute conn insertDecode $ DR p
    return ()
  _ -> return ()
  where
    insertDecode :: Query
    insertDecode = "INSERT INTO decode(client_id,new,snr,delta_time,delta_frequency,mode,message) values (?,?,?,?,?,?,?)"

newtype DR = DR Decode
instance ToRow DR where
  toRow (DR (Decode {..})) = [
      toField decode_client_id
    , toField decode_new
--    , toField decode_time
    , toField decode_snr
    , toField decode_delta_time
    , toField decode_delta_frequency
    , toField decode_mode
    , toField decode_message
    ]

{-
fontaine=> CREATE DATABASE wsjtx;
\c wsjtx
CREATE TABLE decode (client_id text, new boolean, time time, snr int, delta_time double precision , delta_frequency integer, mode text, message text);
-}
