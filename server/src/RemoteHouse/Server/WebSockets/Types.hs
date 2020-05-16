{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RemoteHouse.Server.WebSockets.Types where

import Data.Maybe (fromJust)

import Data.Aeson (FromJSON(..), ToJSON(..), encode, decode)
import Data.Aeson.Types (Value(..), Parser(..), (.:), (.=), object, prependFailure, typeMismatch)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import Network.WebSockets (DataMessage(..), WebSocketsData(..))

import RemoteHouse.Types

data WebSocketsMessage =
  ActOnSeat TableId SeatId
  | RequestHouseInfo
  | MoveToSeat TableId SeatId
  | NotifyHouseInfo UserStore TableMessageStore SeatingChartStore
  | NotifySeatingChartStore SeatingChartStore
  | NotifyTableMessageStore TableMessageStore
  | NotifyUserStore UserStore
  | NotifyPoke UserId
  | PokeSended UserId
  | UpdateTableMessage TableId TableMessage
  | UpdateUserInfo UserName UserImage
  | UpdateUserInfoSucceed
  | UserNotRegistered
  deriving Show

userStoreToObject :: UserStore -> Value
userStoreToObject userStore = object $ flip map (HashMap.toList userStore) $ \(userId, user) ->
  UUID.toText userId .= object ["userName" .= userName user, "userImage" .= userImage user]

tableMessageStoreToObject :: TableMessageStore -> Value
tableMessageStoreToObject tableMessageStore =
  object $ flip map (HashMap.toList tableMessageStore) $ \(tableId, tableMessage) ->
    (Text.pack $ show tableId) .= tableMessage

seatingChartStoreToObject :: SeatingChartStore -> Value
seatingChartStoreToObject seatingChartStore =
  object $ flip map (HashMap.toList seatingChartStore) $ \((table, seat), userId) ->
    UUID.toText userId .= object ["table" .= table, "seat" .= seat]

instance ToJSON WebSocketsMessage where
  toJSON UpdateUserInfoSucceed = object ["type" .= ("updateUserInfoSucceed" :: Text)]
  toJSON (NotifyUserStore userStore) = object
    [ "type" .= ("notifyUserStore" :: Text)
    , "value" .= userStoreToObject userStore
    ]
  toJSON (NotifyTableMessageStore tableMessageStore) = object
    [ "type" .= ("notifyTableMessageStore" :: Text)
    , "value" .= tableMessageStoreToObject tableMessageStore
    ]
  toJSON (NotifySeatingChartStore seatingChartStore) = object
    [ "type" .= ("notifySeatingChartStore" :: Text)
    , "value" .= seatingChartStoreToObject seatingChartStore
    ]
  toJSON (NotifyHouseInfo userStore tableMessageStore seatingChartStore) = object
    [ "type" .= ("notifyHouseInfo" :: Text)
    , "value" .= (object
        [ "userStore" .= userStoreToObject userStore
        , "tableMessageStore" .= tableMessageStoreToObject tableMessageStore
        , "seatingChartStore" .= seatingChartStoreToObject seatingChartStore
        ])
    ]
  toJSON (NotifyPoke userId) = object
    [ "type" .= ("notifyPoke" :: Text)
    , "value" .= object ["from" .= userId]
    ]
  toJSON UserNotRegistered = object ["type" .= ("userNotRegistered" :: Text)]
  toJSON (MoveToSeat tableId seatId) = object
    [ "type" .= ("moveToSeat" :: Text)
    , "value" .= object ["table" .= tableId, "seat" .= seatId]
    ]
  toJSON (PokeSended userId) = object
    [ "type" .= ("pokeSended" :: Text)
    , "value" .= object ["to" .= userId]
    ]

  toJSON message = error ("ToJSON instance of " ++ show message ++ " is unimplemented.")

instance FromJSON WebSocketsMessage where
    parseJSON (Object obj) = do
      type_ <- obj .: "type" :: Parser String
      case type_ of
        "requestHouseInfo" -> pure RequestHouseInfo
        "updateUserInfo" -> do
          v <- obj .: "value"
          UpdateUserInfo
            <$> v .: "userName"
            <*> v .: "userImage"
        "actOnSeat" -> do
          v <- obj .: "value"
          ActOnSeat
            <$> v .: "table"
            <*> v .: "seat"
        "updateTableMessage" -> do
          v <- obj .: "value"
          UpdateTableMessage
            <$> v .: "table"
            <*> v .: "tableMessage"
        _ -> error ("FromJSON instance of " ++ type_ ++ " doesn't implemented.")

    parseJSON invalid    =
        prependFailure "parsing WebSocketsMessage failed, "
            (typeMismatch "Object" invalid)

instance WebSocketsData WebSocketsMessage where
    fromDataMessage (Text   _  (Just tl)) = fromLazyByteString $ encodeUtf8 tl
    fromDataMessage (Text   bl Nothing)   = fromLazyByteString bl
    fromDataMessage (Binary bl)           = fromLazyByteString bl

    fromLazyByteString = fromJust . decode
    toLazyByteString   = encode

