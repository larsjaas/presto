#!/usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi test)
        (presto alist))

(define s '(("vol" . 11) ("band" . "spinal tap") ("members" "david" "nigel" "derek")))

; alist?
(test #t (alist? '(("a" . "b"))))
(test #f (alist? "hei"))
(test #f (alist? 'sym))
(test #f (alist? '("a" "b" "v")))
(test #t (alist? s))
(test #t (alist? '()))

; update-alist
(test '("vol" . 12) (assoc "vol" (update-alist s "vol" 12)))
(test '(("vol" . 11)
        ("band" . "spinal tap")
        ("members" . ("david" "nigel" "derek" "ian")))
      (update-alist s "members" '("david" "nigel" "derek" "ian")))

; patch-alist
(test '(("vol" . 12)
        ("band" . "spinal tap")
        ("members" . ("david" "nigel" "derek"))
        ("manager" . "ian"))
      (patch-alist s '(("vol" . 12) ("manager" . "ian"))))

; alist-unlink
(test '(("vol" . 11) ("band" . "spinal tap")) (alist-unlink s "members"))
(test '(("vol" . 11) ("members" . ("david" "nigel" "derek"))) (alist-unlink s "band"))
(test '() (alist-unlink (alist-unlink (alist-unlink s "vol") "band") "members"))
(test s (alist-unlink s "manager"))

(define (main args)
  (newline))
