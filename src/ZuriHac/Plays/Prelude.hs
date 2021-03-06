module ZuriHac.Plays.Prelude
  ( module X
  , tshow
  ) where

import Prelude as X
import GHC.Stack as X (HasCallStack)
import Data.List as X (isPrefixOf, stripPrefix, foldl', sortBy)
import Data.Char as X (toLower)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Control.Concurrent.STM.TVar as X
import Control.Concurrent.STM as X
import Data.Functor.Identity as X (Identity, runIdentity)
import Data.ByteString as X (ByteString)
import Data.Semigroup as X ((<>))
import Control.Exception.Safe as X
import Debug.Trace as X
import Control.Monad as X (void, guard, foldM, forM_, forM)
import Control.Concurrent.MVar as X
import Data.Ord as X (comparing, Down(..))
import Control.Concurrent as X (threadDelay)
import Control.Concurrent.Async as X (race_)
import Data.Int as X (Int64)

import qualified Data.Text as T

tshow :: Show a => a -> Text
tshow = T.pack . show
