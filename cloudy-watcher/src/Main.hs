{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

coreModified :: Event -> Bool
coreModified (Modified filepath time something) = 
  filepath == "/home/vagrant/.cabal/bin/cloudy-core"
coreModified _ = False 


main =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "/home/vagrant/.cabal/bin"          -- directory to watch
      coreModified -- predicate
      (\x -> do print x ; print "Core Modified")       -- action
    
    --sleep forever (until interrupted)
    forever $ threadDelay 1000000


