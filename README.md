# Introduction

presto is basically a very basic, ad-hoc, extendable, web and rest-api-server
implemented in chibi-scheme.  Maybe one day it will grow up to be something
more.

# Dependencies:

* chibi-scheme: http://synthcode.com/scheme/chibi

# Execution:

    $ export CHIBI_MODULE_PATH=$CHIBI_MODULE_PATH:`pwd`/src/main
    $ chibi-scheme -r src/main/boot.scm

The environment setup has been put in setup.sh which you can source for
simplicity.

    $ . setup.sh
    $ chibi-scheme -r src/main/boot.scm

Sourcing setup.sh and starting presto (as boot.scm) has been put in presto.sh,
so you don't even have to source setup.  You can just run presto.sh.

    $ ./presto.sh

Now that presto is running, point a browser to http://localhost:1080/.

# Testsuite:

Tests are in src/test/ and can be run directly with chibi-scheme, e.g.:

    $ chibi-scheme -r src/test/parse-test.scm

You can also point a browser to http://localhost:1080/testsuite to run
the full test set and see the output in the browser together with timing
info.

# Projects & Development

The development of this project is at the moment driven by the development
of https://github.com/RosenborgSupporterSoftware/storkar, which is based on
a front-end architecture style I am familiar with from work projects, but
here with presto as the scheme-based, written-from-scratch, back-end.
It is also driven by my desire to elevate my scheme-programming skills from
a quite basic level to advanced.

When this front-end is fully functional, focus will return to this back-end
to explore some areas of interest. One plan is to make the request handling
workflow actor-based (like akka actors) in some way or other, to really make
request-handling scalable. I also want to support things like websockets.
With more robustness and a more serious approach to standards compliance,
presto could end up being a very scalable and usable, fun little project.


