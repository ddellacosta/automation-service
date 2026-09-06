# Allure Report

This uses Allure Report (https://allurereport.org) for test reporting. Along with a pretty view this can provide historical analysis of test runs. The following was used to confirm that this works as expected before building out the CI flow to use Allure Report:

(Frontend)

```shell
> cd ui
> spago bundle -p automation-service-test
> node test/run-tests.mjs
# ...tests run in Chromium via Playwright against a local web server;
# Allure results are written to allure-results/ by the test suite itself,
# including a screenshot of the app for failing tests
> cd ..
> allure generate --clean ui/allure-results
Report successfully generated to allure-report
> npx static -p 8080 allure-report
serving "allure-report" at http://127.0.0.1:8080
```

Note that in CI, history/trends are managed automatically by the
publish-reports job; the manual history copying from earlier versions of
this flow is no longer needed.

(Backend)

```shell
> cabal test --test-show-details=always --test-options "--color=always --xml=report.xml"
> rm -rf allure-results
> node test/convert-junit-to-allure.mjs report.xml
Converted 35 testcases to Allure results at allure-results
> cp -r allure-report/history allure-results
> allure generate --clean allure-results
Report successfully generated to allure-report
> npx static -p 8080 allure-report
serving "allure-report" at http://127.0.0.1:8080
```
