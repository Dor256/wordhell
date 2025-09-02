#!/usr/bin/env node

const fs = require('fs');
const { spawn } = require('child_process');

const serverProcess = spawn('zsh', [`${__dirname}/dev.zsh`], { stdio: 'inherit' });

console.log('Starting file watcher...');

let debouncedTimeout = null;
fs.watch('../', { recursive: true }, (_, file) => {
  if (file.endsWith('.elm') || file.endsWith('.hs')) {
    clearTimeout(debouncedTimeout);
    console.log('Detected changes');

    debouncedTimeout = setTimeout(() => {
      const buildProcess = spawn('zsh', [`${__dirname}/dev.zsh`], { stdio: 'inherit' });
      buildProcess.on('exit', () => {
        console.log('Client rebuilt');
      });
    }, 1000);
  }
});
