{-# LANGUAGE OverloadedStrings #-}
module ZuriHac.Plays.DebugSink (run) where

import Pipes

import ZuriHac.Plays.Sink
import ZuriHac.Plays.Prelude

run :: IO ()
run = do
  eventsSourceConnection "localhost" 8000 "0" $ \conn -> runEffect $
    for (eventsProducer conn) (lift . print)
