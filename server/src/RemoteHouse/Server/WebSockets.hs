{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RemoteHouse.Server.WebSockets where

import Control.Exception (catch, throw)
import Control.Monad (forever, when)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, readIORef)
import Data.Maybe (fromJust)

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.HashMap.Strict as HashMap
import Data.UUID.V4 (nextRandom)
import Network.WebSockets (Connection, withPingThread, receiveData, sendTextData, ConnectionException(..))

import RemoteHouse.Types
import RemoteHouse.Server.WebSockets.Types

connectionExceptionHander :: UserId -> RemoteHouseStorage -> ConnectionException -> IO ()
connectionExceptionHander userId RemoteHouseStorage{..} _ = do
  modifyIORef userConnectionStoreRef $ HashMap.delete userId
  modifyIORef userStoreRef $ HashMap.delete userId
  modifyIORef seatingChartStoreRef $ HashMap.filter (/= userId)

broadcast :: RemoteHouseStorage -> WebSocketsMessage -> IO ()
broadcast storage@RemoteHouseStorage{..} message= do
  userConnectionStore <- readIORef userConnectionStoreRef
  for_ (HashMap.toList userConnectionStore) $ \(userId, conn) ->
    sendTextData conn message `catch` connectionExceptionHander userId storage

wsServer :: MonadIO m => RemoteHouseStorage -> Connection -> m ()
wsServer storage@RemoteHouseStorage{..} conn = do
  userId <- liftIO $ do
    userId <- nextRandom
    modifyIORef userConnectionStoreRef $ HashMap.insert userId conn
    pure userId
  liftIO $ withPingThread conn 30 (pure ()) . forever $ do
    message <- receiveData conn :: IO WebSocketsMessage
    case message of
      RequestHouseInfo -> do
        userStore <- readIORef userStoreRef
        tableMessageStore <- readIORef tableMessageStoreRef
        seatingChartStore <- readIORef seatingChartStoreRef
        sendTextData conn $ NotifyHouseInfo userStore tableMessageStore seatingChartStore
      UpdateUserInfo newUserName newUserImage -> do
        let user = User newUserName newUserImage
        modifyIORef userStoreRef $ HashMap.insert userId user
        userStore <- readIORef userStoreRef
        broadcast storage $ NotifyUserStore userStore
        sendTextData conn UpdateUserInfoSucceed
      ActOnSeat table seat -> do
        userStore <- readIORef userStoreRef
        case HashMap.lookup userId userStore of
          Nothing -> sendTextData conn UserNotRegistered
          Just _ -> do
            seatingChartStore <- readIORef seatingChartStoreRef
            case HashMap.lookup (table, seat) seatingChartStore of
              Nothing -> do
                modifyIORef seatingChartStoreRef
                  (HashMap.insert (table, seat) userId . HashMap.filter (/= userId))
                seatingChartStore <- readIORef seatingChartStoreRef
                broadcast storage $ NotifySeatingChartStore seatingChartStore
                sendTextData conn (MoveToSeat table seat)
              Just toUserId -> when (toUserId /= userId) $ do
                userConn <- (fromJust . HashMap.lookup toUserId) <$> (readIORef userConnectionStoreRef)
                sendTextData userConn (NotifyPoke userId) `catch` connectionExceptionHander toUserId storage
                sendTextData conn (PokeSended toUserId)
      UpdateTableMessage tableId tableMessage -> do
        modifyIORef tableMessageStoreRef $ HashMap.insert tableId tableMessage
        tableMessageStore <- readIORef tableMessageStoreRef
        broadcast storage $ NotifyTableMessageStore tableMessageStore
      _ -> error ("WebSockets message (" ++ show message ++ ") is not supported")

