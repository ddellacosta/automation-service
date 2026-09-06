#!/usr/bin/env node
//
// run-tests.mjs — process harness for the frontend test bundle.
//
// `spago bundle -p automation-service-test` compiles the PureScript specs
// into a self-contained ES module (test/test.mjs) that drives a real
// browser via Playwright and exits 0/1. That bundle expects the app under
// test to be served over HTTP (the UI computes its WebSocket URL from the
// page URL). This script is the single entry point for running the tests,
// locally and in CI (nix/frontend-test.nix calls it):
//
//   1. start http-server for the app (serves the CWD, which must contain
//      index.html plus the built app artifacts — index.js and compiled css)
//   2. wait until it is actually serving (avoids goto-vs-server races)
//   3. run the test bundle as a child process, inheriting stdio
//   4. always tear the server down, and exit with the bundle's exit code
//
// NOTE: http-server ignores a trailing positional root argument (observed
// in the nix build sandbox), so the root is passed FIRST.
//
// Environment:
//   TEST_APP_PORT   port to serve on (default: 8850)
//
// Usage: node test/run-tests.mjs [path-to-bundle]
//
import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import http from 'node:http';

const PORT = Number(process.env.TEST_APP_PORT ?? 8850);
const BASE_URL = `http://localhost:${PORT}`;

function die(msg) {
  console.error(`run-tests: ${msg}`);
  process.exit(1);
}

// --- locate the test bundle and the server ---------------------------------
const bundle = process.argv[2] ?? 'test/test.mjs';
if (!existsSync(bundle)) {
  die(`bundle not found at ${bundle}; run \`spago bundle -p automation-service-test\` first`);
}

const serverBin = 'node_modules/.bin/http-server';
if (!existsSync(serverBin)) {
  die("couldn't find http-server; install it with `npm install -D http-server`");
}

// --- start the server ------------------------------------------------------
// -c-1 disables caching so the browser always sees the current files.
const server = spawn(
  process.execPath,
  [serverBin, '.', '-p', String(PORT), '-c-1', '--silent'],
  { stdio: 'inherit' }
);

let child = null;

let serverExited = false;

server.on('exit', (code) => {
  serverExited = true;
  // if the test child hasn't started yet, the server dying is fatal
  if (!child) die(`static file server exited prematurely (code ${code})`);
});

function waitForServer(timeoutMs = 15000) {
  const deadline = Date.now() + timeoutMs;
  return new Promise((resolve, reject) => {
    const attempt = () => {
      if (serverExited) return; // server.on('exit') will have exited already
      const req = http.get(`${BASE_URL}/`, (res) => {
        res.resume(); // drain
        resolve();
      });
      req.on('error', () => {
        if (Date.now() > deadline) {
          reject(new Error(`nothing came up at ${BASE_URL} within ${timeoutMs}ms`));
        } else {
          setTimeout(attempt, 200);
        }
      });
    };
    attempt();
  });
}

// --- run the test bundle ---------------------------------------------------
function cleanup(code) {
  if (!server.killed) server.kill();
  process.exit(code);
}

for (const sig of ['SIGINT', 'SIGTERM']) {
  process.on(sig, () => {
    if (child && !child.killed) child.kill(sig);
    cleanup(130);
  });
}

try {
  await waitForServer();
  console.log(`run-tests: serving '.' at ${BASE_URL}`);
  child = spawn(process.execPath, [bundle], { stdio: 'inherit' });
  const code = await new Promise((resolve, reject) => {
    child.on('error', reject);
    child.on('exit', (code, signal) =>
      resolve(code ?? (signal != null ? 128 + 15 : 1))
    );
  });
  cleanup(code);
} catch (err) {
  console.error(`run-tests: ${err.message}`);
  cleanup(1);
}