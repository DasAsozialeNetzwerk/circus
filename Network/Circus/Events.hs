module Network.Circus.Events (
    Event(..)
  , EventType(..)
  , ircLineToEvent
  ) where

import Network.Circus.IRCMessage (parseIRCMessage, IRCMessage(..))
import Data.Maybe                (fromJust, isNothing)

data Event = Event
  { eType    :: EventType
  , eNick    :: Maybe String
  , eChannel :: Maybe String
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

ircLineToEvent :: String -> Event
ircLineToEvent l = Event
  { eType    = read command
  , eNick    = iNick parsedIRCLine
  , eChannel = channel
  , eArgs    = args
  }
  where parsedIRCLine = fromJust $ parseIRCMessage l
        command       = iCommand parsedIRCLine
        params        = iParams parsedIRCLine
        channel       = if command `elem` [ "PRIVMSG", "JOIN", "TOPIC", "KICK", "NOTICE", "PART", "TOPIC"]
                          then Just $ head params
                          else Nothing
        args          = if isNothing channel then params else tail params
