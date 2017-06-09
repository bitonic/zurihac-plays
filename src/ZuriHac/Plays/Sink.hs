{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.Sink where

import Pipes
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import ZuriHac.Plays.Protocol
import ZuriHac.Plays.Prelude

eventsSourceConnection ::
     String -- ^ URL
  -> Int -- ^ Port
  -> RoomId
  -> (WS.Connection -> IO a)
  -> IO a
eventsSourceConnection host port rid cont = withSocketsDo $ do
  WS.runClient host port (T.unpack ("/room/" <> rid <> "/events-source")) cont

eventsProducer ::
     WS.Connection
  -> Producer Event IO void
eventsProducer conn = loop
  where
    loop = do
      txt <- liftIO (WS.receiveData conn)
      case Aeson.eitherDecode txt of
        Left err -> do
          fail ("Error while decoding: " ++ err)
        Right evt -> yield evt
      loop

