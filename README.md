# Introduction

presto is basically a very basic, ad-hoc, extendable, web and rest-server
implemented in chibi-scheme.  Maybe one day it will grow up to be something
more.

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

# Projects

The development of this project is at the moment driven by the development
of https://github.com/RosenborgSupporterSoftware/storkar, which is based on
a front-end architecture style I am familiar with from work projects, but
here with presto as the scheme-based, written-from-scratch, back-end.

When this front-end is operational, focus will return to this back-end to
explore some areas of interest. The plan is to make the request handling
workflow actor-based (like akka actors) in some way or other. I also want to
support keep-alive connections, as well as websockets. With more robustness
and a more serious approach to standards compliance, this could end up being
a very scalable and usable, fun little project.
