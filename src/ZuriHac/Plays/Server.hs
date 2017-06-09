{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.Server  where

import Solga
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
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
import Data.FileEmbed (embedFile)
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid.V4
import qualified System.Clock as Clock

import ZuriHac.Plays.Prelude
import ZuriHac.Plays.Protocol
import ZuriHac.Plays.KeyVoting

-- Static
-- --------------------------------------------------------------------

clientHtml :: ByteString
clientHtml = $(embedFile "static/client.html")

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
  , cridNext :: RoomId -> TVar RoomState -> TVar RoomSinks -> next
  }
instance forall next. (Router next) => Router (CaptureRoomId next) where
  tryRoute req = case Wai.pathInfo req of
    seg : segs -> do
      roomId <- fromSegment seg
      nextRouter <- tryRoute req{Wai.pathInfo = segs}
      return $ \CaptureRoomId{cridServerStateVar, cridNext} cont -> do
        ss <- atomically (readTVar cridServerStateVar)
        let mbRsVar = HMS.lookup roomId (ssRooms ss)
        case mbRsVar of
          Nothing -> throw (notFound ("Room " <> roomId <> " not found"))
          Just (rst, rsinks) -> nextRouter (cridNext roomId rst rsinks) cont
instance (Abbreviated next) => Abbreviated (CaptureRoomId next) where
  type Brief (CaptureRoomId next) =
   ( TVar ServerState
   , RoomId -> TVar RoomState -> TVar RoomSinks -> Brief next
   )
  brief (ssVar, cont) = CaptureRoomId
    { cridServerStateVar = ssVar
    , cridNext = \rid a b -> brief (cont rid a b)
    }

-- API
-- --------------------------------------------------------------------

type RoomSinks = HMS.HashMap ByteString (Unagi.InChan Event)

data RoomState = RoomState
  { rsUsers :: UserStates
  }

data ServerState = ServerState
  { ssRooms :: HMS.HashMap RoomId (TVar RoomState, TVar RoomSinks)
  , ssRoomIdCounter :: Int
  }

data Api = Api
  { new :: "new" /> Post RoomId
  , client :: "room" /> CaptureRoomId :> "client" /> Method "GET" :> HtmlRaw
  , eventsSink :: "room" /> CaptureRoomId :> "events-sink" /> Method "GET" :> WebSocket
  , eventsSource :: "room" /> CaptureRoomId :> "events-source" /> Method "GET" :> WebSocket
  } deriving (Generic)
instance Router Api

eventsSinkApp :: RoomId -> TVar RoomState -> TVar RoomSinks -> WS.ServerApp
eventsSinkApp rid stateVar sinksVar pendingConn = do
  conn <- WS.acceptRequest pendingConn
  user <- Uuid.toASCIIBytes <$> Uuid.V4.nextRandom
  bracket
    (atomically (modifyTVar stateVar (\rs -> rs{rsUsers = HMS.insert user (UserState mempty mempty) (rsUsers rs)})))
    (\() -> atomically (modifyTVar stateVar (\rs -> rs{rsUsers = HMS.delete user (rsUsers rs)})))
    (\() -> loop conn user)
  where
    loop conn user = do
      txt <- WS.receiveData conn
      case Aeson.eitherDecode txt of
        Left err -> do
          putStrLn ("DECODING ERROR: " ++ err)
        Right msg -> do
          atomically $ do
            rs <- readTVar stateVar
            writeTVar stateVar (RoomState (processEvent (rsUsers rs) user msg))
      loop conn user

eventsSourceApp :: RoomId -> TVar RoomState -> TVar RoomSinks -> WS.ServerApp
eventsSourceApp rid stateVar sinksVar pendingConn = do
  conn <- WS.acceptRequest pendingConn
  sinkId <- Uuid.toASCIIBytes <$> Uuid.V4.nextRandom
  (in_, out) <- Unagi.newChan
  bracket
    (atomically (modifyTVar sinksVar (HMS.insert sinkId in_)))
    (\() -> atomically (modifyTVar sinksVar (HMS.delete sinkId)))
    (\() -> loop conn out)
  where
    loop conn out = do
      evt <- Unagi.readChan out
      WS.sendBinaryData conn (Aeson.encode evt)
      loop conn out

newRoom :: TVar ServerState -> IO RoomId
newRoom ssVar = do
  atomically $ do
    ss <- readTVar ssVar
    let c = ssRoomIdCounter ss
    let rid = tshow c
    rsVar <- newTVar (RoomState mempty)
    rsinksVar <- newTVar mempty
    writeTVar ssVar ss
      { ssRooms = HMS.insert rid (rsVar, rsinksVar) (ssRooms ss)
      , ssRoomIdCounter = c+1
      }
    return rid

clock :: KeysConfig ->  TVar ServerState -> IO a
clock kconf ssVar = loop 0 mempty
  where
    loop :: Clock.TimeSpec -> HMS.HashMap RoomId (HS.HashSet KeyCode) -> IO a
    loop timePassed pressedKcs0 = do
      threadDelay (max 0 (10 * 1000 - (tspecToNano timePassed)))
      t0 <- Clock.getTime Clock.Monotonic
      ss <- atomically (readTVar ssVar)
      pressedKcs <- foldM
        (\pressedKcs (rid, (rstVar, rsinksVar)) -> do
            kcs <- atomically $ do
              RoomState rst <- readTVar rstVar
              let (rst', kcs) = finishRound kconf rst
              writeTVar rstVar (RoomState rst')
              return kcs
            let (pressedKcs', kcsToRelease, kcsToPress) = case HMS.lookup rid pressedKcs of
                  Nothing -> (HMS.insert rid kcs pressedKcs, mempty, kcs)
                  Just kcsPressed -> let
                    toPress = HS.difference kcs kcsPressed
                    toRelease = HS.difference kcsPressed kcs
                    in (HMS.insert rid kcs pressedKcs, toRelease, toPress)
            forM_ (HS.toList kcsToRelease) $ \kc -> do
              sinks <- atomically (readTVar rsinksVar)
              mapM_ (`Unagi.writeChan` EventKeyRelease kc) sinks
            forM_ (HS.toList kcsToPress) $ \kc -> do
              sinks <- atomically (readTVar rsinksVar)
              mapM_ (`Unagi.writeChan` EventKeyPress kc) sinks
            return pressedKcs')
        pressedKcs0
        (HMS.toList (ssRooms ss))
      t1 <- Clock.getTime Clock.Monotonic
      loop (t1 - t0) pressedKcs

    tspecToNano (Clock.TimeSpec sec nsec) = fromIntegral (sec * 1000 + (nsec `div` 1000))

api :: WS.ConnectionOptions -> TVar ServerState -> Api
api wsOptions ssVar = Api
  { new = brief $ do
      newRoom ssVar
  , client = brief
      ( ssVar
      , \_ _ _ -> clientHtml
      )
  , eventsSink = brief
      ( ssVar
      , \roomId rst rsinks -> WebSocket{wsOptions, wsApp = eventsSinkApp roomId rst rsinks}
      )
  , eventsSource = brief
      ( ssVar
      , \roomId rst rsinks -> WebSocket{wsOptions, wsApp = eventsSourceApp roomId rst rsinks}
      )
  }

-- Run
-- --------------------------------------------------------------------

keysConfig :: KeysConfig
keysConfig = KeysConfig
  { kcPercentageRequired = 0.2
  , kcKeyGroups = HMS.fromList
      [ ("ArrowLeft", 1)
      , ("ArrowRight", 1)
      , ("ArrowUp", 2)
      , ("ArrowDown", 3)
      , ("Space", 4)
      ]
  }

run :: IO ()
run = do
  (ssQueueIn, ssQueueOut) <- Unagi.newChan
  ssVar <- newTVarIO ServerState
    { ssRooms = mempty
    , ssRoomIdCounter = 0
    }
  void (newRoom ssVar)
  race_
    (Warp.run 8000 (serve (api WS.defaultConnectionOptions ssVar)))
    (clock keysConfig ssVar)

