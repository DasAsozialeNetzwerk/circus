module Network.Circus.IRCMessage
  ( IRCMessage(..)
  , parseIRCMessage
  ) where

import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Char  (isUpper, isDigit)

data IRCMessage = IRCMessage
  { iServerName :: Maybe String
  , iNick       :: Maybe String
  , iUser       :: Maybe String
  , iHost       :: Maybe String
  , iCommand    :: String
  , iParams     :: [String]
  } deriving (Show, Eq)

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
  | all isDigit $ head cmdParts = Just ("NUMERIC" : tail cmdParts)
  | otherwise                   = Just cmdParts
  where cmdParts = words cmdAndArgs

combineParsed :: Maybe [String] -> Maybe [String] -> Maybe IRCMessage
combineParsed prefixParts cmdParts
  | isNothing prefixParts || isNothing cmdParts = Nothing
  | otherwise = Just IRCMessage
    { iServerName = if onlyServerName then Just $ head $ fromJust prefixParts   else Nothing
    , iNick       = if normalPrefix   then Just $ head $ fromJust prefixParts else Nothing
    , iUser       = if normalPrefix   then Just $ (!! 1) $ fromJust prefixParts else Nothing
    , iHost       = if normalPrefix   then Just $ (!! 2) $ fromJust prefixParts else Nothing
    , iCommand    = head $ fromJust cmdParts
    , iParams     = tail $ fromJust cmdParts
    }
  where prfx = fromMaybe [] prefixParts
        onlyServerName = length prfx == 1
        normalPrefix   = length prfx == 3
