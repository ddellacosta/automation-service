import { chromium } from 'playwright';
import { execSync } from 'child_process';

export const launch_ = (opts) => () => {
  // On NixOS, use the system chromium provided by the flake's devShell.
  // PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH can be set in the shell env,
  // falling back to just "chromium" which works if it's on PATH (which
  // it is in your devShell since pkgs.chromium is included).
  const executablePath =
    process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH ||
    execSync('which chromium').toString().trim();

  // These flags were required by the previous mocha-headless-chrome
  // harness when running under the nix build sandbox / CI runners
  // (see the old nix/frontend-test.nix); without them chromium's
  // renderer silently fails to execute JS in that environment.
  const args = [
    '--no-sandbox',
    '--disable-setuid-sandbox',
    '--disable-dev-shm-usage',
    '--disable-gpu',
  ];

  return chromium.launch({
    ...opts,
    executablePath,
    args,
  });
};

export const click_ = (locator) => () => locator.click();
export const closePage_ = (page) => () => page.close();
export const close_ = (browser) => () => browser.close();
export const evaluate_ = (page) => (js) => () => page.evaluate(new Function(js));
export const fill_ = (locator) => (val) => () => locator.fill(val);
export const goto_ = (page) => (url) => () => page.goto(url);
export const content_ = (page) => () => page.content();
export const inputValue_ = (locator) => () => locator.inputValue();
export const locator_ = (page) => (sel) => () => page.locator(sel);
export const newPage_ = (browser) => () => browser.newPage();
export const nth_ = (locator) => (n) => () => locator.nth(n);
export const textContent_ = (locator) => () => locator.textContent();
export const title_ = (page) => () => page.title();
export const waitForSelector_ = (page) => (sel) => () => page.waitForSelector(sel);
export const pause_ = (page) => () => page.pause();

// Surface page-side console output and JS errors in the test process
// output, so CI logs show what the app was doing (or how it crashed).
export const onConsole_ = (page) => (handler) => () =>
  page.on('console', (msg) => handler(msg.type() + ': ' + msg.text()));

export const onPageError_ = (page) => (handler) => () =>
  page.on('pageerror', (err) => handler('pageerror: ' + err.message));

// status + URL for every response the page receives (404s etc. show up
// here), and network-level request failures
export const onResponse_ = (page) => (handler) => () =>
  page.on('response', (r) => handler('response ' + r.status() + ' ' + r.url()));

export const onRequestFailed_ = (page) => (handler) => () =>
  page.on('requestfailed', (r) =>
    handler('requestfailed ' + r.url() + ' ' +
      ((r.failure() && r.failure().errorText) || '')));
