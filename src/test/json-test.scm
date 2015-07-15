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

(test "{\"name\":\"Ole Kristian Selnæs\",\"short-name\":\"Selnæs\",\"uuid\":\"8303bfa6-a568-40af-b634-934e9cba23cf\",\"image-20-url\":\"http://i.imgur.com/zoWagga.png\"}" (sexp->json '(("name" . "Ole Kristian Selnæs")
 ("short-name" . "Selnæs")
 ("uuid" . "8303bfa6-a568-40af-b634-934e9cba23cf")
 ("image-20-url" . "http://i.imgur.com/zoWagga.png"))))

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
(test '() (json->sexp "{}"))
(test '(("var" . 5)) (json->sexp "{\"var\":5}"))
(test '(("var" . 5)("val" . "hei")) (json->sexp "{\"var\":5, \"val\":\"hei\"}"))
(test '(("var" . (1 2 3))) (json->sexp "{\"var\": [1,2, 3\n]}"))


(define (main args)
  (newline))
