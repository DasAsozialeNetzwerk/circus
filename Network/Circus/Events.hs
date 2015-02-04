module Network.Circus.Events (
    Event(..)
  , EventType(..)
  , EventData(..)
  , EventFunction()
  ) where

data Event = Event
  { eType :: EventType
  , eData :: EventData
  } deriving (Eq, Show)

data EventType =
    Privmsg
  | Ping
  | Join
  | Mode
  | Topic
  | Kick
  | Quit
  | Nick
  | Notice
  | Disconnect
  | Part
  | Numeric
  deriving (Eq, Show)

data EventData = EventData
  { eUser    :: String
  , eChannel :: String
  , eCommand :: String
  , eArgs    :: [String]
  } deriving (Eq, Show)

type EventFunction = Event -> IO ()
