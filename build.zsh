#!/bin/zsh
cd client && elm make src/Main.elm --optimize --output dist/index.js && cd ..
