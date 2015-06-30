# Introduction

presto is basically a very basic, ad-hoc, web server implemented using
chibi-scheme.  Maybe one day it will be something more.

# Dependencies:

* chibi-scheme: http://synthcode.com/scheme/chibi

# Execution:

    $ export CHIBI_MODULE_PATH=$CHIBI_MODULE_PATH:`pwd`/src/main
    $ chibi-scheme -r src/main/boot.scm

Then point a browser to http://loopback:1080/

# Testsuite:

Tests are in src/test/ and can be run directly with chibi-scheme, e.g.:

    $ chibi-scheme src/test/parse.scm

You can also point a browser to http://loopback:1080/testsuite to run
the full test set and see the output in the browser.

