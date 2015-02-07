module Network.Circus.IRCMessage
  ( IRCMessage(..)
  , parseIRCMessage
  ) where

import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Char  (isUpper, isDigit)

-- |The IRCMessage type is a simple representation of an irc message as defined by RFC 2812
data IRCMessage = IRCMessage
  { iServerName :: Maybe String
  , iNick       :: Maybe String
  , iUser       :: Maybe String
  , iHost       :: Maybe String
  , iCommand    :: String
  , iParams     :: [String]
  } deriving (Show, Eq)

-- |parseIRCMessage takes a line that was received from the IRC server and parses it to IRCMessage
-- (to be exact Maybe IRCMessage)
parseIRCMessage :: String -> Maybe IRCMessage
parseIRCMessage line
  | isUpper $ head line = combineParsed (Just []) $ parseCommand line
  | head line == ':'    = combineParsed (parsePrefix line) (parseCommand (tail $ dropWhile (/= ' ') line))
  | otherwise           = Nothing

parsePrefix :: String -> Maybe [String]
parsePrefix line
  | null prefix             = Nothing
  | head prefix /= ':'      = Nothing
  -- probably just a server name
  | '!' `notElem` prefix = Just [tail prefix]
  | not (null nick) &&
    not (null user) &&
    not (null host)         = Just [nick, user, host]
  | otherwise               = Nothing
  where prefix   = takeWhile (/= ' ') line
        nick     = takeWhile (/= '!') $ tail prefix
        user     = takeWhile (/= '@') $ tail $ dropWhile (/= '!') prefix
        host     = tail $ dropWhile (/= '@') prefix

parseCommand :: String -> Maybe [String]
parseCommand cmdAndArgs
  | all isDigit $ head cmdParts = Just ("NUMERIC" : parsedCmdAndArgs)
  | otherwise                   = Just parsedCmdAndArgs
  where cmdParts = words cmdAndArgs
        parsedCmdAndArgs = head cmdParts : parseParams (tail cmdParts)

parseParams :: [String] -> [String]
parseParams splitted = takeWhile firstSpaceNotColon splitted ++ [drop 1 $ unwords $ dropWhile firstSpaceNotColon splitted]
  where firstSpaceNotColon s = head s /= ':'

combineParsed :: Maybe [String] -> Maybe [String] -> Maybe IRCMessage
combineParsed prefixParts cmdParts
  | isNothing prefixParts || isNothing cmdParts = Nothing
  | otherwise = Just IRCMessage
    { iServerName = if onlyServerName then Just $ head $ fromJust prefixParts   else Nothing
    , iNick       = if normalPrefix   then Just $ head $ fromJust prefixParts   else Nothing
    , iUser       = if normalPrefix   then Just $ (!! 1) $ fromJust prefixParts else Nothing
    , iHost       = if normalPrefix   then Just $ (!! 2) $ fromJust prefixParts else Nothing
    , iCommand    = head $ fromJust cmdParts
    , iParams     = tail $ fromJust cmdParts
    }
  where prfx = fromMaybe [] prefixParts
        onlyServerName = length prfx == 1
        normalPrefix   = length prfx == 3
