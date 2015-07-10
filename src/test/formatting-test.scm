#!/usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi test))
(import (presto formatting))

; version-string
(test "1.2" (version-string '(1 2)))
(test "1.2.0" (version-string '(1 2 0)))
(test "1.12.23.2" (version-string '(1 12 23 2)))

; pad-02
(test "00" (pad-02 0))
(test "01" (pad-02 1))
(test "09" (pad-02 9))
(test "10" (pad-02 10))
(test "14" (pad-02 14))
(test "100" (pad-02 100))
(test "0-5" (pad-02 -5))  ; should support this better?

; join
(test "one;2;three" (join ";" '("one" "2" "three")))
(test "a - b - c" (join " - " '("a" "b" "c")))

; url-encode-string
(test "abcdefgh" (url-encode-string "abcdefgh"))
(test "ABCDEFGH" (url-encode-string "ABCDEFGH"))
(test "1234" (url-encode-string "1234"))
(test "+" (url-encode-string " "))
(test "%2b" (url-encode-string "+"))
(test "&" (url-encode-string "&"))
(test "%3f" (url-encode-string "?"))
(test "%23" (url-encode-string "#"))

(define (main args)
  (newline))
