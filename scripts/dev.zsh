#!/bin/zsh

cd ./client && elm make src/Main.elm --output static/dist/index.js
cd ../server

# Only start server if port 3000 is not in use
if ! nc -z localhost 3000 >/dev/null 2>&1; then
  stack run Main.hs &
  SERVER_PID=$!

  # Wait until ready
  until nc -z localhost 3000 >/dev/null 2>&1; do
    sleep 0.3
  done

  open http://localhost:3000
fi
