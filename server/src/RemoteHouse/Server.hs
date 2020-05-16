{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RemoteHouse.Server where

import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings(ssIndices), unsafeToPiece)
import Servant
import Servant.API.WebSocket (WebSocket)

import RemoteHouse.Types
import RemoteHouse.Server.WebSockets

type RemoteHouse =  "ws" :> WebSocket
               :<|> Raw

server :: RemoteHouseStorage -> Server RemoteHouse
server storage = wsServer storage :<|> staticServer
  where
  staticServer =
    let settings = (defaultWebAppSettings "frontend/static") { ssIndices = [unsafeToPiece "index.html"] }
     in serveDirectoryWith settings

app :: RemoteHouseStorage -> Application
app storage = serve (Proxy @RemoteHouse) (server storage)

