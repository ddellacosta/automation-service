#!/usr/bin/env node
//
// run-tests.mjs — process harness for the frontend test bundle.
//
// The test suite itself is a generated artifact (see `bundle:` in
// ui/test/spago.yaml): `spago bundle -p automation-service-test` compiles
// the PureScript specs into a single self-contained ES module (test.mjs)
// that drives a real browser via Playwright and exits 0/1. That bundle
// expects the app under test to be served over HTTP (the UI computes its
// WebSocket URL from the page URL, and the integration test asserts on the
// port embedded in outgoing messages).
//
// This script is the single entry point for running the tests, both
// locally and in CI (nix/frontend-test.nix calls it):
//
//   1. start an off-the-shelf static file server (http-server) for the
//      current directory (run this from ui/, or set TEST_APP_ROOT)
//   2. wait until it is actually serving (avoids goto-vs-server races)
//   3. run the test bundle as a child process, inheriting stdio
//   4. always tear the server down, and exit with the bundle's exit code
//
// Usage:
//   node test/run-tests.mjs [path-to-bundle]
//
// Environment:
//   TEST_APP_PORT          port to serve on (default: 8850)
//   TEST_APP_ROOT          directory to serve (default: .)
//   TEST_BUNDLE            path to the test bundle (default: see below)
//   TEST_HTTP_SERVER_BIN   http-server binary override (default:
//                          ./node_modules/.bin/http-server, then $PATH)
//
import { spawn, execSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import http from 'node:http';
import path from 'node:path';

const PORT = Number(process.env.TEST_APP_PORT ?? 8850);
const BASE_URL = `http://localhost:${PORT}`;

function die(msg) {
  console.error(`run-tests: ${msg}`);
  process.exit(1);
}

// --- locate the test bundle -----------------------------------------------
// The nix build places the bundle differently than a local `spago bundle`
// run (outfile is relative to the spago invocation dir), so check both.
const bundle =
  process.argv[2] ??
  process.env.TEST_BUNDLE ??
  ['test.mjs', 'test/test.mjs'].find(existsSync);

if (!bundle) {
  die(
    'could not find the test bundle; run `spago bundle -p automation-service-test` ' +
      'first, or pass the bundle path as the first argument'
  );
}

// --- locate the static file server ----------------------------------------
function findHttpServer() {
  if (process.env.TEST_HTTP_SERVER_BIN) return process.env.TEST_HTTP_SERVER_BIN;
  const localBin = path.resolve('node_modules/.bin/http-server');
  if (existsSync(localBin)) return localBin;
  try {
    return execSync('which http-server', { encoding: 'utf8' }).trim();
  } catch {
    return null;
  }
}

const serverBin = findHttpServer();
if (!serverBin) {
  die(
    "couldn't find http-server; install it with `npm install -D http-server` " +
      '(or set TEST_HTTP_SERVER_BIN)'
  );
}

// --- start the server ------------------------------------------------------
// -c-1 disables caching; the browser should always see the current files.
const server = spawn(
  process.execPath,
  // root first, then options — per http-server's documented usage
  // (`http-server [path] [options]`); passing the path as a trailing
  // positional was observed to be ignored in the nix build sandbox
  [serverBin, process.env.TEST_APP_ROOT ?? '.', '-p', String(PORT), '-c-1', '--silent'],
  { stdio: 'inherit' }
);

let serverExited = false;
server.on('exit', (code) => {
  serverExited = true;
  // if we haven't started the test child yet, the server dying is fatal
  if (!child) {
    die(`static file server exited prematurely (code ${code})`);
  }
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
let child = null;

function cleanup(code) {
  if (!server.killed) server.kill();
  process.exit(code);
}

for (const sig of ['SIGINT', 'SIGTERM']) {
  process.on(sig, () => {
    if (child && !child.killed) child.kill(sig);
    cleanup(128 + 2); // 130 for SIGINT, same for TERM: close enough
  });
}

try {
  await waitForServer();
  console.log(`run-tests: serving ${process.env.TEST_APP_ROOT ?? '.'} at ${BASE_URL}`);
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