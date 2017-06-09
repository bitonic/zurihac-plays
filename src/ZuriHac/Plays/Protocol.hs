module ZuriHac.Plays.Protocol where

import ZuriHac.Plays.TH
import ZuriHac.Plays.Prelude

{-

{
  "client": <uuid>,
  "event": {
    "KeyPress": <key-code>
  }
}

OR

{
  "client": <uuid>,
  "event": {
    "KeyRelease": <key-code>
  }
}

-}

type KeyCode = Int

data Event
  = EventKeyPress KeyCode
  | EventKeyRelease KeyCode
  deriving (Eq, Show)

type RoomId = Text

deriveJSON (jsonSum "Event") ''Event
