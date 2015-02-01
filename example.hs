import Network.Circus

handleEvent :: String -> IO ()
handleEvent message = do
   print message

main = do
   connect server
   where server = (defaultParams "irc.physicsporn.org" handleEvent) { pChannels = ["#lobby"] }
