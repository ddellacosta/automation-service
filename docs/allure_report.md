# Allure Report

This uses Allure Report (https://allurereport.org) for test reporting. Along with a pretty view this can provide historical analysis of test runs. The following was used to confirm that this works as expected before building out the CI flow to use Allure Report:

(Frontend)

```shell
> cd ui
> spago bundle -p automation-service-test; npx mocha-headless-chrome -t 60000 -e (which chromium) -a 'allow-file-access-from-files' -f test/browser/index.html -r json -o test-output.json
> rm -rf allure-results
> node test/convert-mocha-to-allure.mjs test-output.json
Wrote 4 Allure test results to allure-results
> cd ..
> cp -r allure-report/history ui/allure-results
> allure generate --clean ui/allure-results
Report successfully generated to allure-report
> npx static -p 8080 allure-report
serving "allure-report" at http://127.0.0.1:8080
```

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
