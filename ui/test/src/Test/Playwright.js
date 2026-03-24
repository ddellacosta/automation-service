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

  return chromium.launch({
    ...opts,
    executablePath,
  });
};

export const click_ = (locator) => () => locator.click();
export const closePage_ = (page) => () => page.close();
export const close_ = (browser) => () => browser.close();
export const evaluate_ = (page) => (js) => () => page.evaluate(new Function(js));
export const fill_ = (locator) => (val) => () => locator.fill(val);
export const goto_ = (page) => (url) => () => page.goto(url);
export const inputValue_ = (locator) => () => locator.inputValue();
export const locator_ = (page) => (sel) => () => page.locator(sel);
export const newPage_ = (browser) => () => browser.newPage();
export const nth_ = (locator) => (n) => () => locator.nth(n);
export const textContent_ = (locator) => () => locator.textContent();
export const title_ = (page) => () => page.title();
export const waitForSelector_ = (page) => (sel) => () => page.waitForSelector(sel);
export const pause_ = (page) => () => page.pause();
