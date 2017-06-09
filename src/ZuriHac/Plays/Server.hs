{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.Server (run) where

import Solga
import qualified Data.HashMap.Strict as HMS
import qualified Lucid as L
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Http
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai.Handler.Warp as Warp

import ZuriHac.Plays.Prelude

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

type RoomId = Text

data Api = Api
  { new :: "new" /> Post RoomId
  , client :: "room" /> Capture RoomId :> Method "GET" :> HtmlRaw
  } deriving (Generic)
instance Router Api

data RoomState = RoomState

data ServerState = ServerState
  { ssRooms :: HMS.HashMap RoomId (TVar RoomState)
  , ssRoomIdCounter :: Int
  }

api :: TVar ServerState -> Api
api ssVar = Api
  { new = brief $ atomically $ do
        ss <- readTVar ssVar
        let c = ssRoomIdCounter ss
        rsVar <- newTVar RoomState
        let rid = tshow c
        writeTVar ssVar ss
          { ssRooms = HMS.insert rid rsVar (ssRooms ss)
          , ssRoomIdCounter = c+1
          }
        return rid
  , client = brief (\roomId -> "Hello world " <> T.encodeUtf8 roomId)
  }

run :: IO ()
run = do
  ssVar <- newTVarIO ServerState
    { ssRooms = mempty
    , ssRoomIdCounter = 0
    }
  Warp.run 8000 (serve (api ssVar))

--
-- /new POST => room id
-- /room/<room-id>/events-sink WebSocket
-- /room/<room-id>/events-source WebSocket
-- /room/<room-id>/client GET
