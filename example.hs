import Network.Circus

handleEvent :: EventFunction
handleEvent = print 

main :: IO Session
main = connect server
  where server = (defaultParams "irc.physicsporn.org" handleEvent) { pChannels = ["#AsozialesNetzwerk"] }
