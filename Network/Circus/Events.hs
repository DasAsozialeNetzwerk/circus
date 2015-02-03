module Network.Circus.Events (
    Event(..)
  , EventType(..)
  , EventData(..)
  ) where

data Event = Event
  { eType :: EventType
  , eData :: EventData
  }

data EventType = Privmsg
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

data EventData = EventData
  { eUser    :: String
  , eCommand :: String
  , eArgs    :: [String]
  }
