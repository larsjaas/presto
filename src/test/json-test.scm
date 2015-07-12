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

; repeat-count
(test 0 (repeat-count '(#\a #\b #\c) #\f))
(test 0 (repeat-count '() #\f))
(test 1 (repeat-count '(#\f) #\f))
(test 4 (repeat-count '(#\f #\f #\f #\f) #\f))
(test 2 (repeat-count '(#\f #\f #\e #\f) #\f))
(test 2 (repeat-count '(#\f #\f #\e) #\f))

; json->sexp
(test 5 (json->sexp "5"))
(test 1235 (json->sexp "1235"))
(test -42 (json->sexp "-42"))
(test 1235 (json->sexp "  \n1235"))
(test "" (json->sexp "\"\""))
(test "a" (json->sexp "\"a\""))
(test "a\"" (json->sexp "\"a\\\"\"")) ; ugly!
(test #t (json->sexp " true"))
(test #f (json->sexp "false"))
(test #t (json->sexp "true,"))
(test #f (json->sexp "  false]"))
(test '() (json->sexp " null\n"))
(test '() (json->sexp "[]"))
(test '(1 2) (json->sexp "[1,2]"))
(test '(1 "2" 3) (json->sexp "[1,\"2\",3]"))
(test '(1 (2 3)) (json->sexp "[1,[2,3]]"))

; (test '() (json->sexp "{}"))
; (test '() (json->sexp "()"))

(define (main args)
  (newline)
  ;(display (json->sexp "12")) (newline)
  ;(display (json->sexp "[]")) (newline)
  ;(display (json->sexp "[1,2]")) (newline)
  ;(display (json->sexp "\"a\\\"\"")) (newline)
  ;(display (json->sexp " \"a\"")) (newline)
  ;(display (json->sexp "[1,2,3]")) (newline)
  ;(display (json->sexp "[1,2]")) (newline)
  ;(display (json->sexp "[1,[2,3]]")) (newline)
  ;(display (sexp->json '(("key1" . "value1") ("key2" . "value2"))))
  (newline))
