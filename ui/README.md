# automation-service UI

Currently pinned to React 17.0.2 as that is what Elmish uses.

## Bundling for deployment

```shell
> watchexec -w src -- npx spago bundle -p automation-service
```

## Testing

```shell
> watchexec -w src -w test/src -- "npx spago bundle -p automation-service-test; npx mocha-headless-chrome -t 60000 -e (which chromium) -a 'allow-file-access-from-files' -f test/browser/index.html"

# with non-headless browser:
> watchexec -w src -w test/src -- "npx spago bundle -p automation-service-test; npx mocha-headless-chrome -v -t 60000 -e (which chromium) -a 'allow-file-access-from-files' -f test/browser/index.html"

```

