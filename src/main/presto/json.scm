; TODO:
; - read full range of number types, not just integers
; - decode encoded unicode characters in strings
; - pretty formatting of json output

(define *elog* '())

(define (json-initialize)
  (set! *elog* (get-error-log-logger)))

(define (alist->json obj)
  (cond ((alist? obj)
          (apply string-append
            (cons "{"
                  (let iter ((l obj))
                    (cond ((null? l)
                            (cons "}" '()))
                          ((pair? (cdr l))
                            (cons (sexp->json (car (car l)))
                                  (cons ":"
                                        (cons (sexp->json (cdr (car l)))
                                              (cons ","
                                                    (iter (cdr l)))))))
                          (else
                            (cons (sexp->json (car (car l)))
                                  (cons ":"
                                        (cons (sexp->json (cdr (car l)))
                                              (iter (cdr l))))))
                    )))))
        (else (*elog* 'error "alist->json called with non-alist!"))))

(define (list->json obj)
  (cond ((list? obj)
          (apply string-append
            (cons "["
                  (let iter ((l obj))
                    (cond ((null? l)
                            (cons "]" '()))
                          ((pair? (cdr l))
                            (cons (sexp->json (car l))
                                  (cons "," (iter (cdr l)))))
                          (else
                            (cons (sexp->json (car l))
                                  (iter (cdr l))))
                    )))))
        (else (*elog* 'error "list->json called with non-list!"))))

(define (string->json obj)
  (cond ((string? obj) ; fixme: quote " and \ and probably unicode characters
          (string-append "\"" obj "\""))
        (else (*elog* 'error "string->json called with non-string!"))))

(define (integer->json obj)
  (cond ((integer? obj)
          (number->string obj))
        (else (*elog* 'error "integer->json called with non-integer!"))))

(define (sexp->json sexp)
  (cond ((null? sexp) ; list/alist tests will trigger further down and break
          "{}")
        ((eq? sexp #t)
          "true")
        ((eq? sexp #f)
          "false")
        ((and (list? sexp) (alist? (car sexp)))
          (list->json sexp))
        ((alist? sexp)
          (alist->json sexp))
        ((list? sexp)
          (list->json sexp))
        ((string? sexp)
          (string->json sexp))
        ((integer? sexp)
          (integer->json sexp))
        (else
          (*elog* 'error "sexp->json called with unsupported expression type '" sexp "'.")
          "???")))

(define (begins-with pat vec pos)
  (let iter ((p pos))
    (cond ((>= (- p pos) (vector-length pat))
            #t)
          ((>= p (vector-length vec))
            #f)
          ((char=? (vector-ref vec p) (vector-ref pat (- p pos)))
            (iter (+ p 1)))
          (else
            #f))))

(define (string-dequote str)
  (let ((chars '())
        (special #f))
    (string-for-each
      (lambda (c)
        (cond (special
                (set! special #f)
                (cond ((char=? c #\n)
                        (set! chars (cons #\newline chars)))
                      ((char=? c #\r)
                        (set! chars (cons #\return chars)))
                      ((char=? c #\t)
                        (set! chars (cons #\tab chars)))
                      ((char=? c #\b)
                        (set! chars (cons (integer->char 8) chars)))
                      ((char=? c #\f)
                        (set! chars (cons (integer->char 12) chars)))
                      ((char=? c #\u) ; just preserve the unicode encoding
                        (set! chars (cons #\\ chars))
                        (set! chars (cons c chars)))
;                     ((char=? c #\u)
;                       ... read 4 hex digits as unicode character
                      (else
                        (set! chars (cons c chars)))))
              ((char=? c #\\)
                (set! special #t))
              (else
                (set! chars (cons c chars)))))
      str)
    (list->string (reverse chars))))

(define (repeat-count lst c)
  (let iter ((l lst)
             (i 0))
    (cond ((null? l) i)
          ((char=? (car l) c)
            (iter (cdr l) (+ i 1)))
          (else i))))

(define (char-separator? c)
  (or (char-whitespace? c)
      (char=? c #\,)
      (char=? c #\:)
      (char=? c #\])
      (char=? c #\})))

(define (json->sexp json)
  (if (not (string? json))
      (raise "do not call json->sexp with a non-string object"))

  (let* ((data (string->vector json))
         (end-idx (vector-length data)))

    (define (advance continue? i)
      (let loop ((pos i)
                 (eaten '()))
        (cond ((>= pos end-idx)
                (list->string (reverse eaten)))
              ((continue? (cons (vector-ref data pos) eaten)
                          (- pos i))
                (loop (+ pos 1) (cons (vector-ref data pos) eaten)))
              (else
                (list->string (reverse eaten))))))

    (define (read-number-from idx)
      (define (continue? c n)
         (or (char-numeric? (car c))
             (and (= n 0) (char=? (car c) #\-))))
      (let ((val (advance continue? idx)))
        (cons (string->number val) (string-length val))))

    (define (read-string-from idx)
      (define (continue? c n)
        (or (not (char=? (car c) #\"))
            (and (char=? (car c) #\")
                 (= 1 (modulo (repeat-count (cdr c) #\\) 2)))))
      (let ((val (advance continue? (+ idx 1))))
        (cons (string-dequote val) (+ (string-length val) 2))))

    (define (read-array-from idx)
      (let ((array '()))
        (let iter ((i idx))
          (cond ((>= i end-idx)
                  (raise (string-append "invalid json at char "
                                        (number->string i))))
                ((char-whitespace? (vector-ref data i))
                  (iter (+ i 1)))
                ((char=? (vector-ref data i) #\])
                  (cons (reverse array) (- i idx -2)))
                (else
                  (let ((readval (read-value-from i)))
                    (set! array (cons (car readval) array))
                    (let scan ((pos (+ i (cdr readval)))) ; scan until , or ]
                      (cond ((>= pos end-idx)
                              '())
                            ((char-whitespace? (vector-ref data pos))
                              (scan (+ pos 1)))
                            ((char=? (vector-ref data pos) #\,)
                              (iter (+ pos 1)))
                            ((char=? (vector-ref data pos) #\])
                              (iter pos))
                            (else
                              (raise (string-append "invalid json at char "
                                                    (number->string pos))))))))
                ))))

    (define (read-object-from idx)
      (let ((object '()))
        (let iter ((i idx))
          (cond ((>= i end-idx)
                  (raise (string-append "invalid json at char "
                                        (number->string i))))
                ((char-whitespace? (vector-ref data i))
                  (iter (+ i 1)))
                ((char=? (vector-ref data i) #\})
                  (cons (reverse object) (- i idx -1)))
                ((char=? (vector-ref data i) #\")
                  (let ((stringval (read-string-from i)))
                    (let scan ((pos (+ i (cdr stringval))))
                      (cond ((>= pos end-idx)
                              (raise (string-append "invalid json at char "
                                                    (number->string pos))))
                            ((char-whitespace? (vector-ref data pos))
                              (scan (+ pos 1)))
                            ((char=? (vector-ref data pos) #\:)
                              (let ((readval (read-value-from (+ pos 1))))
                                (set! object (cons (cons (car stringval) (car readval))
                                                   object))
                                (let cont ((pos2 (+ pos (cdr readval) 1)))
                                  (cond ((>= pos2 end-idx)
                                          (raise (string-append "invalid json at pos "
                                                                (number->string pos2))))
                                        ((char-whitespace? (vector-ref data pos2))
                                          (cont (+ pos2 1)))
                                        ((char=? (vector-ref data pos2) #\,)
                                          (iter (+ pos2 1)))
                                        ((char=? (vector-ref data pos2) #\})
                                          (iter pos2))
                                        (else
                                          (raise (string-append "invalid json at char "
                                                                (number->string pos2))))))))
                            (else
                              (raise (string-append "invalid json at char "
                                                    (number->string pos))))
                            ))))

                (else
                  (raise (string-append "invalid json at char "
                         (number->string i))))
                ))))

    (define (read-value-from idx)
      (let iter ((i idx))
        (cond ((>= i end-idx)
                (cons '() (- i idx)))
              ((char-whitespace? (vector-ref data i))
                (iter (+ i 1)))
              ((or (char=? (vector-ref data i) #\-)
                   (char-numeric? (vector-ref data i)))
                (let ((readval (read-number-from i)))
                  readval))
              ((char=? (vector-ref data i) #\")
                (let ((readval (read-string-from i)))
                  readval))
              ((and (begins-with #(#\t #\r #\u #\e) data i)
                    (or (>= (+ i 4) end-idx)
                        (char-separator? (vector-ref data (+ i 4)))))
                (cons #t 4))
              ((and (begins-with #(#\f #\a #\l #\s #\e) data i)
                    (or (>= (+ i 5) end-idx)
                        (char-separator? (vector-ref data (+ i 5)))))
                (cons #f 5))
              ((and (begins-with #(#\n #\u #\l #\l) data i)
                    (or (>= (+ i 4) end-idx)
                        (char-separator? (vector-ref data (+ i 4)))))
                (cons '() 4))
              ((char=? (vector-ref data i) #\[)
                (let ((readval (read-array-from (+ i 1))))
                  (cons (car readval) (+ (- i idx) (cdr readval)))))
              ((char=? (vector-ref data i) #\{)
                (let ((readval (read-object-from (+ i 1))))
                  (cons (car readval) (+ (- i idx) (cdr readval)))))
              (else
                (raise (string-append "invalid json at char "
                                      (number->string i)))))))

    (let ((data (read-value-from 0)))
      (car data))))

(define (json-prettify json)
  (let ((indent 0)
        (instring #f)
        (builder '()))
    (string-for-each
      (lambda (c)
        (cond ((and instring
                    (char=? c #\")
                    (= (modulo (repeat-count builder #\\) 2) 0))
                (set! builder (cons c builder))
                (set! instring #f))
              (instring
                (set! builder (cons c builder)))
              ((char=? c #\")
                (set! builder (cons c builder))
                (set! instring #t))
              ((char=? c #\:)
                (set! builder (cons #\space (cons c builder))))
              ((char=? c #\,)
                (set! builder (cons #\newline (cons c builder)))
                (set! builder
                  (let iter ((i indent) (b builder))
                    (cond ((> i 0)
                            (cons #\space (iter (- i 1) b)))
                          (else
                            b)))))
              ((or (char=? c #\[) (char=? c #\{))
                (set! indent (+ indent 2))
                (set! builder (cons #\newline (cons c builder)))
                (set! builder
                  (let iter ((i indent) (b builder))
                    (cond ((> i 0)
                            (cons #\space (iter (- i 1) b)))
                          (else
                            b)))))
              ((or (char=? c #\]) (char=? c #\}))
                (set! indent (- indent 2))
                (set! builder (cons #\newline builder))
                (set! builder
                  (let iter ((i indent) (b builder))
                    (cond ((> i 0)
                            (cons #\space (iter (- i 1) b)))
                          (else
                            b))))
                (set! builder (cons c builder)))
              (else
                (set! builder (cons c builder)))))
      json)
    (list->string (reverse (cons #\newline builder)))))

;  json)
