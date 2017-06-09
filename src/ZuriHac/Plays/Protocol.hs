module ZuriHac.Plays.Protocol where

import ZuriHac.Plays.TH
import ZuriHac.Plays.Prelude

data Message = Message
  { messageClient :: Text
  , messageEvent :: Event
  }

type KeyCode = Int

data Event
  = EventKeyPress KeyCode
  | EventKeyRelease KeyCode

deriveJSON (jsonProduct "message") ''Message
deriveJSON (jsonSum "Event") ''Event

