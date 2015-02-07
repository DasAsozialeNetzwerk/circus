import Network.Circus

handleEvent :: EventFunction
handleEvent = print 

main :: IO Session
main = connect server
  where server = (defaultParams "irc.freenode.net" handleEvent) { pNick = "circus-hs", pChannels = ["#circus-hs"] }
