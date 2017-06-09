{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.Server  where

import Solga
import qualified Data.HashMap.Strict as HMS
import qualified Lucid as L
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import qualified Control.Concurrent.Chan.Unagi as Unagi

import ZuriHac.Plays.Prelude
import ZuriHac.Plays.Protocol

-- Routers
-- --------------------------------------------------------------------

newtype Html = Html (L.Html ())
  deriving (Generic)

instance Router Html where
  tryRoute _ = Just $ \(Html html) cont ->
    cont (Wai.responseBuilder Http.status200 headers (runIdentity (L.execHtmlT html)))
      where
        headers = [(Http.hContentType, "text/html")]

newtype HtmlRaw = HtmlRaw ByteString
  deriving (Generic)
instance Router HtmlRaw where
  tryRoute _ = Just $ \(HtmlRaw rawHtml) cont ->
    cont (Wai.responseLBS Http.status200 headers (BSL.fromChunks [rawHtml]))
      where
        headers = [(Http.hContentType, "text/html")]
instance Abbreviated HtmlRaw where
  type Brief HtmlRaw = ByteString
  brief = HtmlRaw

data WebSocket = WebSocket
  { wsApp :: WS.ServerApp
  , wsOptions :: WS.ConnectionOptions
  }
instance Router WebSocket where
  tryRoute req = Just $ \WebSocket{wsApp, wsOptions} cont -> do
    let mbResp = WS.websocketsApp wsOptions wsApp req
    case mbResp of
      Nothing -> error "TODO Handle web browser with no WS"
      Just resp -> cont resp
      where
        headers = [(Http.hContentType, "text/html")]
instance Abbreviated WebSocket

data CaptureRoomId next = CaptureRoomId
  { cridServerStateVar :: TVar ServerState
  , cridNext :: RoomId -> RoomChans -> next
  }
instance forall next. (Router next) => Router (CaptureRoomId next) where
  tryRoute req = case Wai.pathInfo req of
    seg : segs -> do
      roomId <- fromSegment seg
      nextRouter <- tryRoute req
      return $ \CaptureRoomId{cridServerStateVar, cridNext} cont -> do
        ss <- atomically (readTVar cridServerStateVar)
        let mbRsVar = HMS.lookup roomId (ssRooms ss)
        case mbRsVar of
          Nothing -> throw (notFound ("Room " <> roomId <> " not found"))
          Just rsVar -> nextRouter (cridNext roomId rsVar) cont
instance (Abbreviated next) => Abbreviated (CaptureRoomId next) where
  type Brief (CaptureRoomId next) =
         ( TVar ServerState
         , RoomId -> RoomChans -> Brief next
         )
  brief (ssVar, cont) = CaptureRoomId
    { cridServerStateVar = ssVar
    , cridNext = \rid rsVar -> brief (cont rid rsVar)
    }

-- API
-- --------------------------------------------------------------------

type RoomId = Text

data RoomChans = RoomChans
  { rcIn :: Unagi.InChan Message
  , rcOut :: Unagi.OutChan Message
  }

data ServerState = ServerState
  { ssRooms :: HMS.HashMap RoomId RoomChans
  , ssRoomIdCounter :: Int
  }

data Api = Api
  { new :: "new" /> Post RoomId
  , client :: "room" /> CaptureRoomId :> "client" /> Method "GET" :> HtmlRaw
  , eventsSink :: "room" /> CaptureRoomId :> "events-sink" /> Method "GET" :> WebSocket
  , eventsSource :: "room" /> CaptureRoomId :> "events-source" /> Method "GET" :> WebSocket
  } deriving (Generic)
instance Router Api

eventsSinkApp :: RoomId -> Unagi.InChan Message -> WS.ServerApp
eventsSinkApp rid in_ pendingConn = do
  loop =<< WS.acceptRequest pendingConn
  where
    loop conn = do
      txt <- WS.receiveData conn
      case Aeson.eitherDecode txt of
        Left err -> do
          putStrLn ("DECODING ERROR: " ++ err)
        Right msg -> do
          Unagi.writeChan in_ msg
      loop conn

eventsSourceApp :: RoomId -> Unagi.OutChan Message -> WS.ServerApp
eventsSourceApp rid out pendingConn = do
  loop =<< WS.acceptRequest pendingConn
  where
    loop conn = do
      msg <- Unagi.readChan out
      WS.sendBinaryData conn (Aeson.encode msg)
      loop conn

api :: WS.ConnectionOptions -> TVar ServerState -> Api
api wsOptions ssVar = Api
  { new = brief $ do
      (rcIn, rcOut) <- Unagi.newChan
      atomically $ do
        ss <- readTVar ssVar
        let c = ssRoomIdCounter ss
        let rid = tshow c
        writeTVar ssVar ss
          { ssRooms = HMS.insert rid RoomChans{rcIn, rcOut} (ssRooms ss)
          , ssRoomIdCounter = c+1
          }
        return rid
  , client = brief
      ( ssVar
      , \_roomId _rsVar -> T.encodeUtf8 "Hello"
      )
  , eventsSink = brief
      ( ssVar
      , \roomId rsChans -> WebSocket{wsOptions, wsApp = eventsSinkApp roomId (rcIn rsChans)}
      )
  , eventsSource = brief
      ( ssVar
      , \roomId rsChans -> WebSocket{wsOptions, wsApp = eventsSourceApp roomId (rcOut rsChans)}
      )
  }

-- Run
-- --------------------------------------------------------------------

run :: IO ()
run = do
  (ssQueueIn, ssQueueOut) <- Unagi.newChan
  ssVar <- newTVarIO ServerState
    { ssRooms = mempty
    , ssRoomIdCounter = 0
    }
  Warp.run 8000 (serve (api WS.defaultConnectionOptions ssVar))

