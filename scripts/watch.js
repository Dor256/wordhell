#!/usr/bin/env node

const fs = require('fs');
const { spawn, execSync } = require('child_process');

let process = spawn(`${__dirname}/dev.zsh`, [], { stdio: 'inherit' });
console.log('Starting file watcher...');

let debouncedTimeout = null;

fs.watch('../', { recursive: true }, (_, file) => {
  if (file.endsWith('.elm') || file.endsWith('.hs')) {
    clearTimeout(debouncedTimeout);

    console.log('Detected changes');

    debouncedTimeout = setTimeout(() => {
      execSync('killall server');
      process = spawn(`${__dirname}/dev.zsh`, [], { stdio: 'inherit' });
    }, 1000);
  }
});
