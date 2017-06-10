{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.Sink where

import Pipes
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Control.Retry as Retry
import qualified Control.Monad.Catch as E

import ZuriHac.Plays.Protocol
import ZuriHac.Plays.Prelude

wsRetryPolicy :: Retry.RetryPolicy
wsRetryPolicy = Retry.constantDelay (25 * 1000)

-- | Automatically retries on connection failures
eventsSourceConnection ::
     String -- ^ URL
  -> Int -- ^ Port
  -> RoomId
  -> (forall void. WS.Connection -> IO void)
  -> IO void
eventsSourceConnection host port rid cont = withSocketsDo $ do
  Retry.recovering wsRetryPolicy
    [ \_ -> E.Handler $ \(e :: IOError) -> do
        putStrLn ("GOT IOError: " ++ show e)
        return True
    ]
    (\_ -> WS.runClient host port (T.unpack ("/room/" <> rid <> "/events-source")) $ \conn -> do
        WS.forkPingThread conn 1
        cont conn)

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

