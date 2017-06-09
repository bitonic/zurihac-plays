module ZuriHac.Plays.XSink (run) where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (forever)
import           ZuriHac.Plays.Prelude
import qualified ZuriHac.Plays.XSink.FakeKey as FakeKey

run :: IO ()
run = do
    FakeKey.init
    forever $ do
        FakeKey.press 65
        threadDelay $ 500 * 1000
        FakeKey.release 65
        threadDelay $ 500 * 1000
