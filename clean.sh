#!/bin/bash
cd cloudy-datatypes
cabal clean
cd ..
cd cloudy-front
cabal clean
cd ..
cd cloudy-core
cabal clean
cd ..
cd cloudy-web 
cabal clean
cd ..
cd cloudy-watcher
cabal clean
cd ..
echo Cloudy Mud Cleaned and Ready for a Commit!
