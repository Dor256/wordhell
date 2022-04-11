# Requirements

- This project uses `Haskell Stack` for the server side and `Elm` for the client side.
- *DO NOT* use any independent `stack` or `elm` commands to run this project, instead use the provided shell scripts inside the `scripts` folder.
- `Node` is required only if there is need for the development server to refresh on file changes.

# Scripts
- `start-dev.zsh` - Start the development server with the file watcher and restart on file changes.
- `scripts/dev.zsh` - Start the development server without the file watcher.
- `scripts/watch.js` - (There is no need to use this) Directly start the file watcher.
- `build.zsh` - Build a production version of the app.

# Missing Features
- ### Make guesses persistent
