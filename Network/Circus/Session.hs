module Network.Circus.Session
  ( Params(..)
  , Session(..)
  , defaultParams
  , connect
  , write
  , identify
  , listen
  ) where

import Network
import System.IO
import Control.Monad

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
   , pEvent :: String -> IO ()
   , pDebug :: Bool
   }

defaultParams :: String -> (String -> IO ()) -> Params
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
   listen session
   return session
   where addr = pAddr params
         port = PortNumber (fromIntegral $ pPort params)

write :: Session -> String -> IO ()
write session message = hPutStr (sSocket session) (message ++ "\r\n")

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
   pEvent (sParams session) line
   listen session

