#!/usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi string)
        (chibi test))
(import (presto parse))

; string-split
(test '("one" "2" "three") (string-split "one 2 three"))
(test '("on" " 2 thr" "" "") (string-split "one 2 three" #\e))

; path-split
(test '("images/" "image" ".png") (path-split "images/image.png"))
(test '("/" "README" ".txt") (path-split "/README.txt"))
(test '("/" "README") (path-split "/README"))
(test '(#f "README") (path-split "README"))
(test '(#f "" ".config") (path-split ".config"))
(test '("/home/larsa/" "" ".bashrc") (path-split "/home/larsa/.bashrc"))
(test '(#f "image" ".png") (path-split "image.png"))
(test '(#f "archive" ".tar" ".gz") (path-split "archive.tar.gz"))
(test '("/home/larsa/temp/" "presto-0" ".0" ".1" ".tar" ".gz")
      (path-split "/home/larsa/temp/presto-0.0.1.tar.gz"))
(test '("/tmp/temp.001/" "file" ".tmp") (path-split "/tmp/temp.001/file.tmp"))

; url-decode
(test '"a" (url-decode "a"))
(test '" " (url-decode "+"))
(test '" " (url-decode "%20"))
(test '" / " (url-decode "%20/%20"))

(define (main args)
  (newline))
