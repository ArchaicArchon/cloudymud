#!/bin/bash
cabal update
cd cloudy-datatypes
cabal install
cd ..
cd cloudy-front
cabal install
cd ..
echo Cloudy Front Installed!
