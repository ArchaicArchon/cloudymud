#!/bin/bash
cabal update
cd cloudy-datatypes
cabal install
cd ..
cd cloudy-front
cabal install
cd ..
cd cloudy-core
cabal install
cd ..
cd cloudy-web 
cabal install
cd ..
cd cloudy-watcher
cabal install
cd ..
echo Cloudy Mud Installed!
