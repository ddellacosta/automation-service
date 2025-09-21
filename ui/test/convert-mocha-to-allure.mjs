//
// This was written by ChatGPT 5.0 and should be considered disposable
// code. -DD
//
// Usage: node tools/convert-mocha-to-allure.mjs test_output.json [allure-results-dir]
import fs from "fs";
import path from "path";
import crypto from "crypto";

const [,, inputPath = "test_output.json", resultsDir = "allure-results"] = process.argv;

function ensureDir(p) {
  fs.mkdirSync(p, { recursive: true });
}

function md5(s) {
  return crypto.createHash("md5").update(String(s)).digest("hex");
}

function safeFilename(s) {
  return String(s || "")
    .replace(/[^\w.-]+/g, "_")
    .slice(0, 160);
}

function nowMs() {
  return Date.now();
}

function hasErrorObject(err) {
  if (!err || typeof err !== "object") return false;
  // Consider it an error only if it has some content we care about
  const keys = Object.keys(err);
  if (keys.length === 0) return false;
  return Boolean(err.message || err.stack || err.name || err.actual !== undefined || err.expected !== undefined);
}

function determineStatus(test, passSet, failSet) {
  // Prefer Mocha's own state if present
  if (test.pending || test.state === "pending") return "skipped";
  if (test.state === "failed") return "failed";
  if (test.state === "passed") return "passed";

  // Fall back to membership in passes/failures arrays
  const key = (test.fullTitle || test.title || "").trim();
  if (failSet.has(key)) return "failed";
  if (passSet.has(key)) return "passed";

  // Finally, infer from err contents
  if (hasErrorObject(test.err)) return "failed";

  return "passed";
}

function toAllureTestResult(test, status, runId) {
  const fullName = (test.fullTitle || test.title || "").trim();
  const duration = Number(test.duration || 0);
  const stop = nowMs();
  const start = stop - (isFinite(duration) ? duration : 0);

  const historyId = md5(`${test.file || ""}::${fullName}`);
  const uuid = md5(`${historyId}:${runId}:${Math.random()}`);

  const labels = [
    { name: "framework", value: "mocha" },
    { name: "suite", value: "frontend" },
    { name: "language", value: "javascript" },
  ];
  if (test.file) labels.push({ name: "package", value: test.file });

  const statusDetails = hasErrorObject(test.err)
    ? {
        message: String(test.err.message || ""),
        trace: String(test.err.stack || ""),
      }
    : undefined;

  return {
    uuid,
    historyId,
    testCaseId: historyId,
    name: test.title || fullName || "Unnamed test",
    fullName: fullName || (test.title || "Unnamed test"),
    status,
    stage: "finished",
    statusDetails,
    labels,
    links: [],
    steps: [],
    parameters: [],
    attachments: [],
    start,
    stop,
  };
}

function writeExecutor(resultsDir) {
  const exec = {
    name: process.env.GITHUB_WORKFLOW ? "GitHub Actions" : "CI",
    type: process.env.GITHUB_WORKFLOW ? "github" : "generic",
    buildName: process.env.GITHUB_WORKFLOW || "CI",
    buildOrder: Number(process.env.GITHUB_RUN_NUMBER || 0),
    buildUrl:
      process.env.GITHUB_SERVER_URL &&
      process.env.GITHUB_REPOSITORY &&
      process.env.GITHUB_RUN_ID
        ? `${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}/actions/runs/${process.env.GITHUB_RUN_ID}`
        : "",
    reportName: "Allure Report",
    url:
      process.env.GITHUB_SERVER_URL && process.env.GITHUB_REPOSITORY
        ? `${process.env.GITHUB_SERVER_URL}/${process.env.GITHUB_REPOSITORY}`
        : "",
  };
  fs.writeFileSync(path.join(resultsDir, "executor.json"), JSON.stringify(exec, null, 2));
}

function main() {
  if (!fs.existsSync(inputPath)) {
    console.error(`Input not found: ${inputPath}`);
    process.exit(1);
  }
  const text = fs.readFileSync(inputPath, "utf8");
  let report;
  try {
    report = JSON.parse(text);
  } catch (e) {
    console.error("Failed to parse Mocha JSON:", e.message);
    process.exit(1);
  }

  const tests = Array.isArray(report.tests) ? report.tests : [];
  const pending = Array.isArray(report.pending) ? report.pending : [];
  const passes = Array.isArray(report.passes) ? report.passes : [];
  const failures = Array.isArray(report.failures) ? report.failures : [];

  const passSet = new Set(passes.map(t => (t.fullTitle || t.title || "").trim()));
  const failSet = new Set(failures.map(t => (t.fullTitle || t.title || "").trim()));

  ensureDir(resultsDir);
  writeExecutor(resultsDir);

  const runId =
    process.env.GITHUB_RUN_ID ||
    process.env.BUILD_ID ||
    String(Date.now());

  // Combine tests + pending (pending may also be present in tests with pending: true)
  const seen = new Set();
  const allTests = [];
  for (const t of tests) {
    const key = (t.fullTitle || t.title || "").trim();
    if (!seen.has(key)) {
      allTests.push(t);
      seen.add(key);
    }
  }
  for (const p of pending) {
    const key = (p.fullTitle || p.title || "").trim();
    if (!seen.has(key)) {
      allTests.push({ ...p, pending: true }); // ensure pending flag
      seen.add(key);
    }
  }

  let count = 0;
  for (const t of allTests) {
    const status = determineStatus(t, passSet, failSet);
    const ar = toAllureTestResult(t, status, runId);
    const fileName = `${safeFilename(ar.fullName)}-${ar.uuid.slice(0, 8)}-result.json`;
    fs.writeFileSync(path.join(resultsDir, fileName), JSON.stringify(ar, null, 2));
    count++;
  }

  // Optional environment.properties to surface versions in the report
  const envPropsPath = path.join(resultsDir, "environment.properties");
  const envProps = [
    `NODE=${process.version}`,
    `MOCHA=${report?.meta?.mochaVersion || ""}`,
    `TESTS_TOTAL=${report?.stats?.tests || allTests.length}`,
    `TESTS_PASS=${report?.stats?.passes || passes.length}`,
    `TESTS_FAIL=${report?.stats?.failures || failures.length}`,
    `TESTS_PENDING=${report?.stats?.pending || pending.length}`,
  ].join("\n");
  fs.writeFileSync(envPropsPath, envProps);

  console.log(`Wrote ${count} Allure test results to ${resultsDir}`);
}

main();