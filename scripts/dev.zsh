#!/bin/zsh

cd ./client && elm make src/Main.elm --output dist/index.js && cd ../server && stack run Main.hs && cd ..
