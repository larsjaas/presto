#!/usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi test))
(import (presto http 1.1))

; http/1.1-status-message
(test "OK" (http/1.1-status-message 200))
(test "Not Found" (http/1.1-status-message 404))

; http/1.1-status-line
(test "HTTP/1.1 200 OK" (http/1.1-status-line 200))
(test "HTTP/1.1 404 Not Found" (http/1.1-status-line 404))

; http/1.1-date-format
(test "Tue, 07 Jul 2015 13:58:52 GMT" (http/1.1-date-format 1436270332))

; TODO: http/1.1-date-parse

(define (main args)
  (newline))
