import crypto from 'crypto';
import fs from 'fs';
import path from 'path';

export const writeFileSync_ = (filePath) => (contents) => () => {
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  fs.writeFileSync(filePath, contents, 'utf8');
};

export const prepareResultsDir = (dir) => () => {
  fs.rmSync(dir, { recursive: true, force: true });
  fs.mkdirSync(dir, { recursive: true });
};

// Pure (deterministic, no side effects), hence exposed to PureScript as
// a pure function rather than wrapped in Effect.
export const md5Hash = (s) =>
  crypto.createHash('md5').update(s).digest('hex');

// Keep only filesystem-safe characters (GitHub artifact upload rejects
// filenames containing e.g. '>', which shows up in test names like
// "Main app > Can navigate to different pages").
export const safeFilename_ = (s) =>
  s.replace(/[^\w.-]+/g, '_').slice(0, 120);
