import           Data.Maybe     (fromJust)
import           Network.Circus

handleEvent :: EventFunction
handleEvent _ ev = case eType ev of
                      Privmsg -> putStrLn $ fromJust (eNick ev) ++ " said in " ++ fromJust (eChannel ev) ++ ": " ++ head (eArgs ev)
                      _      -> print ev

main :: IO Session
main = connect server
  where server = (defaultParams "irc.freenode.net" handleEvent) { pNick = "circus-hs", pChannels = ["#circus-hs"] }
