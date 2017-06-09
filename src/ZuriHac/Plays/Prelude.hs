module ZuriHac.Plays.Prelude
  ( module X
  , tshow
  ) where

import Prelude as X
import GHC.Stack as X (HasCallStack)
import Data.List as X (isPrefixOf, stripPrefix)
import Data.Char as X (toLower)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Control.Concurrent.STM.TVar as X (TVar, readTVar, writeTVar, newTVar, newTVarIO)
import Control.Concurrent.STM as X (atomically)
import Data.Functor.Identity as X (Identity, runIdentity)
import Data.ByteString as X (ByteString)
import Data.Semigroup as X ((<>))
import Control.Exception.Safe as X (throw)

import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show
