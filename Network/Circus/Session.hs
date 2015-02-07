module Network.Circus.Session
  ( Params(..)
  , Session(..)
  , defaultParams
  , connect
  , write
  , identify
  , listen
  ) where

import Network                (connectTo, PortID(..))
import Network.Circus.Events  (EventFunction(), ircLineToEvent)
import System.IO              (Handle, BufferMode(..), hSetBuffering, hIsEOF, hGetLine, hPutStr)
import Control.Monad          (when)
import Data.List              (isPrefixOf, stripPrefix, intercalate)
import Data.Maybe             (fromJust)

data Session = Session
   { sParams :: Params
   , sSocket :: Handle
   }

data Params = Params
   { pAddr :: String
   , pPort :: Int
   , pNick :: String
   , pUser :: String
   , pReal :: String
   , pChannels :: [String]
   -- this will be a list later
   , pEvent :: EventFunction
   , pDebug :: Bool
   }

defaultParams :: String -> EventFunction -> Params
defaultParams addr eventfn = Params
   { pAddr = addr
   , pPort = 6667
   , pNick = "circus"
   , pUser = "circus"
   , pReal = "circus"
   , pChannels = []
   , pEvent = eventfn
   , pDebug = True
   }

connect :: Params -> IO Session
connect params = do
   h <- connectTo addr port
   hSetBuffering h NoBuffering
   let session = Session { sParams = params, sSocket = h }
   _ <- identify session
   _ <- joinChannels session (pChannels params)
   listen session
   return session
   where addr = pAddr params
         port = PortNumber (fromIntegral $ pPort params)

write :: Session -> String -> IO ()
write session message = hPutStr (sSocket session) (message ++ "\r\n")

joinChannels :: Session -> [String] -> IO Session
joinChannels session channels = do
   write session $ (++) "JOIN " (intercalate ", " channels)
   return session

identify :: Session -> IO Session
identify session = do
   write session $ "NICK " ++ nick
   write session $ "USER " ++ user ++ " 0 * :" ++ real
   return session
   where nick = pNick (sParams session)
         user = pUser (sParams session)
         real = pReal (sParams session)

listen :: Session -> IO ()
listen session = do
   eof <- hIsEOF (sSocket session)
   when eof $ return ()

   line <- hGetLine (sSocket session)
   when ("PING :" `isPrefixOf` line) (write session ((++) "PONG :" $ fromJust $ stripPrefix "PING :" line))
   pEvent (sParams session) $ ircLineToEvent line
   listen session

