#!/usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi test)
        (presto json))

; alist?
(test #f (alist? ""))
(test #t (alist? '()))
(test #f (alist? '("hei" "paa" "deg")))
(test #t (alist? '(("hei" . 1) ("paa" . 2) ("deg" . 3))))
(test #f (alist? '(("hei" . 1) ("paa") "deg")))
(test #t (alist? '(("hei" . 1) ("paa"))))

; sexp->json
(test "{}" (sexp->json '()))
(test "[1,2,3]" (sexp->json '(1 2 3)))
(test "{\"key\":\"value\"}" (sexp->json '(("key" . "value"))))
(test "{\"key1\":\"value1\",\"key2\":\"value2\"}"
      (sexp->json '(("key1" . "value1") ("key2" . "value2"))))
(test "[true,false]" (sexp->json '(#t #f)))

(define (main args)
  ;(newline)
  ;(display (sexp->json '(("key1" . "value1") ("key2" . "value2"))))
  (newline))
