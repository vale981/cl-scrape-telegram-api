# CL-SCRAPE-TELEGRAM-API

Scrapes the online [api reference](https://core.telegram.org/bots/api)
for methods an type definitions.  The scraped methods and types are
transformed into lisp `functions` and `classes` to be used in the
`cl-telegram` package.

## Usage
 1. Use the command line utility `generate-api.ros [output-file]
[api-url]` with both args optional.
 2. Load the system in the repl and execute `(tg-scrape:scrape-to-disk)`.
    ```common-lisp
    (ql:quickload :cl-scrape-telegram-api)
    (tg-scrape:scrape-to-disk :out-file "file.lisp")
    ```

## Todo
 - Generate type checking for arrays
 - Generate constructors
