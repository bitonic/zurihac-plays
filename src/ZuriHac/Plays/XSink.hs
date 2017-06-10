{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.XSink (run) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (forever, when)
import           Pipes

import           ZuriHac.Plays.Prelude
import           ZuriHac.Plays.Protocol
import qualified ZuriHac.Plays.XSink.FakeKey as FakeKey
import           ZuriHac.Plays.Sink

run :: IO ()
run = do
  FakeKey.init
  eventsSourceConnection "localhost" 8000 "0" $ \conn -> runEffect $
    for (eventsProducer conn) (lift . playEvent)

keyCodeToX11KeySym :: KeyCode -> Maybe Int
keyCodeToX11KeySym code = case code of
  "KeyA" -> Just 0x0061
  "KeyB" -> Just 0x0062
  "KeyC" -> Just 0x0063
  "KeyD" -> Just 0x0064
  "KeyE" -> Just 0x0065
  "KeyF" -> Just 0x0066
  "KeyG" -> Just 0x0067
  "KeyH" -> Just 0x0068
  "KeyI" -> Just 0x0069
  "KeyJ" -> Just 0x006a
  "KeyK" -> Just 0x006b
  "KeyL" -> Just 0x006c
  "KeyM" -> Just 0x006d
  "KeyN" -> Just 0x006e
  "KeyO" -> Just 0x006f
  "KeyP" -> Just 0x0070
  "KeyQ" -> Just 0x0071
  "KeyR" -> Just 0x0072
  "KeyS" -> Just 0x0073
  "KeyT" -> Just 0x0074
  "KeyU" -> Just 0x0075
  "KeyV" -> Just 0x0076
  "KeyW" -> Just 0x0077
  "KeyX" -> Just 0x0078
  "KeyY" -> Just 0x0079
  "KeyZ" -> Just 0x007a
  "ArrowLeft" -> Just 0xff51
  "ArrowUp" -> Just 0xff52
  "ArrowRight" -> Just 0xff53
  "ArrowDown" -> Just 0xff54
  "Space" -> Just 0x0020
  _ -> Nothing


playEvent :: Event -> IO ()
playEvent event = do
  case keyCodeToX11KeySym (ekpKeyCode event) of
    Nothing -> return ()
    Just keysym -> do
      case event of
        EventKeyPress{} -> FakeKey.press (fromIntegral keysym)
        EventKeyRelease{} -> FakeKey.release (fromIntegral keysym)
