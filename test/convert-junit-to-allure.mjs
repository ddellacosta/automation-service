// Usage:
//   node tools/convert-junit-to-allure.mjs path/to/report.xml [allure-results-dir]
//
// Example:
//   node tools/convert-junit-to-allure.mjs backend/report.xml backend/allure-results
//
// Then generate:
//   allure generate backend/allure-results --clean -o backend/allure-report
//
// Or merge with frontend results from the browser tests:
//   allure generate ui/allure-results backend/allure-results --clean -o allure-report

import fs from "node:fs";
import path from "node:path";
import crypto from "node:crypto";
import { XMLParser } from "fast-xml-parser";

const [,, inputXml = "report.xml", outDir = "allure-results"] = process.argv;

function ensureDir(p) {
  fs.mkdirSync(p, { recursive: true });
}

function md5(s) {
  return crypto.createHash("md5").update(String(s)).digest("hex");
}

function safeFilename(s) {
  return String(s || "").replace(/[^\w.-]+/g, "_").slice(0, 160);
}

function toMs(secondsLike) {
  if (secondsLike == null) return 0;
  const n = Number(secondsLike);
  if (!isFinite(n)) return 0;
  return Math.max(0, Math.round(n * 1000));
}

function arrayify(x) {
  if (x == null) return [];
  return Array.isArray(x) ? x : [x];
}

function collectTestcases(root) {
  // Handles both <testsuites> and <testsuite> roots
  const suites = [];

  if (root.testsuites) {
    const ts = arrayify(root.testsuites.testsuite ?? root.testsuites);
    for (const s of ts) suites.push(s);
  } else if (root.testsuite) {
    const ts = arrayify(root.testsuite);
    for (const s of ts) suites.push(s);
  } else {
    // Some producers put test cases at root directly
    if (root.testcase) suites.push({ name: "", testcase: root.testcase });
  }

  const items = [];
  for (const suite of suites) {
    const suiteName = suite.name || "";
    const cases = arrayify(suite.testcase);
    for (const tc of cases) {
      items.push({ suiteName, testcase: tc });
    }
  }
  return items;
}

function extractStatus(tc) {
  // Map according to JUnit semantics:
  // - <failure> => assertion failure -> failed
  // - <error> => infra/error -> broken
  // - <skipped> => skipped
  // - otherwise passed
  const failures = arrayify(tc.failure);
  const errors = arrayify(tc.error);
  const skipped = arrayify(tc.skipped);

  if (skipped.length > 0) return { status: "skipped", details: extractDetails(skipped[0]) };
  if (failures.length > 0) return { status: "failed", details: extractDetails(failures[0]) };
  if (errors.length > 0) return { status: "broken", details: extractDetails(errors[0]) };
  return { status: "passed", details: undefined };
}

function extractDetails(node) {
  if (!node || typeof node !== "object") return undefined;
  // fast-xml-parser puts element text in "#text" by default
  const message = node.message || node.type || "";
  const text = node["#text"] || "";
  const combined = [message, text].filter(Boolean).join("\n");
  return combined
    ? {
        message: String(message || ""),
        trace: String(text || ""),
      }
    : undefined;
}

function buildAllureResult({ suiteName, testcase }, runId) {
  const name = testcase.name || "Unnamed test";
  const className = testcase.classname || "";
  const fullName = [className, name].filter(Boolean).join("::") || name;

  const durationMs = toMs(testcase.time);
  const stop = Date.now();
  const start = stop - durationMs;

  const { status, details } = extractStatus(testcase);

  const idBase = `${className}::${name}`;
  const historyId = md5(idBase);
  const uuid = md5(`${historyId}:${runId}:${Math.random()}`);

  const labels = [
    { name: "framework", value: "junit" },
    { name: "suite", value: suiteName || className || "backend" },
  ];
  if (className) labels.push({ name: "package", value: className });

  return {
    uuid,
    historyId,
    testCaseId: historyId,
    name,
    fullName,
    status, // passed | failed | skipped | broken
    stage: status === "skipped" ? "pending" : "finished",
    statusDetails: details,
    labels,
    links: [],
    steps: [],
    parameters: [],
    attachments: [],
    start,
    stop,
  };
}

function writeExecutor(outDir) {
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
  fs.writeFileSync(path.join(outDir, "executor.json"), JSON.stringify(exec, null, 2));
}

function main() {
  if (!fs.existsSync(inputXml)) {
    console.error(`Input not found: ${inputXml}`);
    process.exit(1);
  }

  const xml = fs.readFileSync(inputXml, "utf8");
  const parser = new XMLParser({
    attributeNamePrefix: "",
    ignoreAttributes: false,
    allowBooleanAttributes: true,
    parseAttributeValue: true,
    trimValues: true,
    ignoreDeclaration: false,
  });

  let root;
  try {
    root = parser.parse(xml);
  } catch (e) {
    console.error("Failed to parse JUnit XML:", e?.message || e);
    process.exit(1);
  }

  const items = collectTestcases(root);
  ensureDir(outDir);
  writeExecutor(outDir);

  const runId = process.env.GITHUB_RUN_ID || process.env.BUILD_ID || String(Date.now());

  let count = 0;
  for (const it of items) {
    const result = buildAllureResult(it, runId);
    const fileName = `${safeFilename(result.fullName)}-${result.uuid.slice(0, 8)}-result.json`;
    fs.writeFileSync(path.join(outDir, fileName), JSON.stringify(result, null, 2));
    count++;
  }

  // Optional: write environment.properties with summary
  const envProps = [
    `NODE=${process.version}`,
    `CI=${process.env.CI ? "true" : "false"}`,
    `TESTS_TOTAL=${items.length}`,
  ].join("\n");
  fs.writeFileSync(path.join(outDir, "environment.properties"), envProps);

  console.log(`Converted ${count} testcases to Allure results at ${outDir}`);
}

main();