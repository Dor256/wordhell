#!/bin/zsh

cd client && elm make src/Main.elm --output dist/index.js && cd .. && cd server && yarn start && cd ..
