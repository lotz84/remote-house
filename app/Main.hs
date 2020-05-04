module Main where

import Network.Wai.Handler.Warp (run)

import System.Environment (lookupEnv, getArgs)

import RemoteHouse.Server (app)
import RemoteHouse.Types (newRemoteHouseStorage)

main :: IO ()
main = do
  args <- getArgs
  let port = if length args > 0 then read (head args) else 8080
  storage <- newRemoteHouseStorage
  run port (app storage)
