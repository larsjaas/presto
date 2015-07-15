(define (command)
  (list "uuidgen")) ; Works on OSX at least

(define (gen-uuid)
  (string-map (lambda (c) (char-downcase c))
              (utf8->string (process->bytevector (command)))))
