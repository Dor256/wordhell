#!/bin/zsh

cd ./client &&
elm make src/Main.elm --optimize --output static/dist/index.js &&
cd ..
