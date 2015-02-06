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
                           "PRIVMSG"    -> [(Privmsg, "")]
                           "JOIN"       -> [(Join, "")] 
                           "MODE"       -> [(Mode, "")]
                           "TOPIC"      -> [(Topic, "")]
                           "KICK"       -> [(Kick, "")]
                           "QUIT"       -> [(Quit, "")]
                           "NICK"       -> [(Nick, "")]
                           "NOTICE"     -> [(Notice, "")]
                           "DISCONNECT" -> [(Disconnect, "")]
                           "PART"       -> [(Part, "")]
                           "NUMERIC"    -> [(Numeric, "")]
                           "PING"       -> [(Ping, "")]
                           _            -> []

type EventFunction = Event -> IO ()
