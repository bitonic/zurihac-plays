{-# LANGUAGE ForeignFunctionInterface #-}
module ZuriHac.Plays.XSink.FakeKey
    ( init
    , press
    , release
    ) where

import           Foreign.C.Types       (CInt (..))
import           ZuriHac.Plays.Prelude hiding (init)

foreign import ccall "fakekey_init"    c_fakekey_init    :: IO ()
foreign import ccall "fakekey_press"   c_fakekey_press   :: CInt -> IO ()
foreign import ccall "fakekey_release" c_fakekey_release :: CInt -> IO ()

init :: IO ()
init = c_fakekey_init

press :: Int -> IO ()
press = c_fakekey_press . fromIntegral

release :: Int -> IO ()
release = c_fakekey_release . fromIntegral
