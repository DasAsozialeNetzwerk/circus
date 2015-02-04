module Network.Circus.Events (
    Event(..)
  , EventType(..)
  , EventFunction()
  ) where

data Event = Event
  { eType    :: EventType
  , eUser    :: String
  , eChannel :: String
  , eArgs    :: [String]
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

type EventFunction = Event -> IO ()
