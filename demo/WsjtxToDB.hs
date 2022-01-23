----------------------------------------------------------------------------
-- |
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Testapplication
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String (fromString)
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

import Network.Socket

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import WSJTX.UDP.NetworkMessage
import WSJTX.UDP.Server (forkWsjtxServer, withWsjtxSocket)

wsjtxPort :: PortNumber
wsjtxPort = 2237

wsjtxAddr :: HostAddress
wsjtxAddr = tupleToHostAddress (0,0,0,0)

main :: IO ()
main = do
  getArgs >>= \case
    [ sqlCommand ] -> runServer wsjtxAddr wsjtxPort (fromString sqlCommand)
    _ -> printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "Usage : wsjtx-to-db SQLCMD"
  putStrLn "Example 'INSERT INTO wsjtx(packet) values (?)'"

runServer :: HostAddress -> PortNumber -> Query -> IO ()
runServer addr port sqlCommand = do
  conn <- connectPostgreSQL "" -- Reads from ENV: PGHOST,PGUSER,PGPASSWORD
  void $ withWsjtxSocket (addr, port) $ \sock -> do
    void $ forkWsjtxServer sock (dbWrite conn sqlCommand)
    forever $ threadDelay 100000000
  return ()

dbWrite :: Connection -> Query -> Packet -> IO ()
dbWrite conn cmd packet
  = void $ execute conn cmd $ Only $ toJSONField packet
