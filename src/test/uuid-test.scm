#!/usr/bin/env chibi-scheme -r

(import (chibi) (chibi io) (chibi test) (chibi string) (presto uuid))

(test #t (string? (gen-uuid)))
(test #t (>= (string-length (gen-uuid)) 16))
(test 5 (length (string-split (gen-uuid) #\-)))

(define (main args)
  (newline))
