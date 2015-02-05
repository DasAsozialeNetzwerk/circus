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
  deriving (Eq)

instance Show EventType where
  show Privmsg    = "PRIVMSG"
  show Join       = "JOIN"
  show Mode       = "MODE"
  show Topic      = "TOPIC"
  show Kick       = "KICK"
  show Quit       = "QUIT"
  show Nick       = "NICK"
  show Notice     = "NOTICE"
  show Disconnect = "DISCONNECT"
  show Part       = "PART"
  show Numeric    = "NUMERIC"
  show Ping       = "PING"

instance Read EventType where
  readsPrec _ input = case input of
                           "PRIVMSG"    -> [(Privmsg, drop (length input) input)]
                           "JOIN"       -> [(Join, drop (length input) input)]
                           "MODE"       -> [(Mode, drop (length input) input)]
                           "TOPIC"      -> [(Topic, drop (length input) input)]
                           "KICK"       -> [(Kick, drop (length input) input)]
                           "QUIT"       -> [(Quit, drop (length input) input)]
                           "NICK"       -> [(Nick, drop (length input) input)]
                           "NOTICE"     -> [(Notice, drop (length input) input)]
                           "DISCONNECT" -> [(Disconnect, drop (length input) input)]
                           "PART"       -> [(Part, drop (length input) input)]
                           "NUMERIC"    -> [(Numeric, drop (length input) input)]
                           "PING"       -> [(Ping, drop (length input) input)]
  readsPrec _ _ = []

type EventFunction = Event -> IO ()
