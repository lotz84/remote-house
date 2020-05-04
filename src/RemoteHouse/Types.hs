{-# LANGUAGE RecordWildCards #-}

module RemoteHouse.Types where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Network.WebSockets (Connection)

type UserName = Text

type UserImage = Text

data User = User
  { userName  :: UserName
  , userImage :: UserImage
  }
  deriving Show

type UserId = UUID

type TableId = Int

type SeatId = Int

type TableMessage = Text

type UserConnectionStore = HashMap UserId Connection

type UserStore = HashMap UserId User

type TableMessageStore = HashMap TableId TableMessage

type SeatingChartStore = HashMap (TableId, SeatId) UserId

data RemoteHouseStorage = RemoteHouseStorage
  { userConnectionStoreRef :: IORef UserConnectionStore
  , userStoreRef :: IORef UserStore
  , tableMessageStoreRef :: IORef TableMessageStore
  , seatingChartStoreRef :: IORef SeatingChartStore
  }

newRemoteHouseStorage :: IO RemoteHouseStorage
newRemoteHouseStorage = do
  newUserConnectionStore <- newIORef HashMap.empty
  newUserStore <- newIORef HashMap.empty
  newTableMessageStore <- newIORef HashMap.empty
  newSeatingChartStore <- newIORef HashMap.empty
  pure $ RemoteHouseStorage newUserConnectionStore newUserStore newTableMessageStore newSeatingChartStore

