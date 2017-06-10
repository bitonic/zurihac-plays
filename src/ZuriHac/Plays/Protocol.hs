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

type KeyCode = Text

data Event
  = EventKeyPress KeyCode
  | EventKeyRelease KeyCode
  deriving (Eq, Show)

ekpKeyCode :: Event -> KeyCode
ekpKeyCode (EventKeyPress keyCode) = keyCode
ekpKeyCode (EventKeyRelease keyCode) = keyCode

type RoomId = Text

deriveJSON (jsonSum "Event") ''Event
