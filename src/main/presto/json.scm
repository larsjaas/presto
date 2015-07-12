
(define *elog* '())

(define (json-initialize)
  (set! *elog* (get-error-log-logger)))

(define (alist? obj)
  (let iter ((l obj))
    (cond ((null? l) #t)
          ((or (not (pair? l)) (not (pair? (car l)))) #f)
          (else
            (iter (cdr l))))))

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
  (cond ;((null? sexp)
        ;  "{}")
        ((eq? sexp #t)
          "true")
        ((eq? sexp #f)
          "false")
        ((alist? sexp) ; triggers on null as well
          (alist->json sexp))
        ((list? sexp)
          (list->json sexp))
        ((string? sexp)
          (string->json sexp))
        ((integer? sexp)
          (integer->json sexp))
        (else
          (*elog* 'error "sexp->json called with unsupported expression type")
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
                      (else
                        (set! chars (cons c chars)))))
              ((char=? c #\\)
                (set! special #t))
              (else
                (set! chars (cons c chars)))))
      str)
    (list->string (reverse chars))))

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

    (define (read-from idx)
      (let iter ((i idx))
        (cond ((>= i end-idx)
                (cons '() (- i idx)))
              ((char=? (vector-ref data i) #\space)
                (iter (+ i 1)))
              ((char=? (vector-ref data i) #\newline)
                (iter (+ i 1)))
              ((or (char=? (vector-ref data i) #\-)
                   (char-numeric? (vector-ref data i)))
                (let ((val (advance (lambda (c n)
                                      (or (char-numeric? (car c))
                                          (and (= n 0) (char=? (car c) #\-))))
                                    i)))
                  (cons (string->number val) (string-length val))))
              ((char=? (vector-ref data i) #\")
                (let ((val (advance (lambda (c n)
                                      (or (not (char=? (car c) #\"))
                                          (and (> n 0)
                                               (char=? (car c) #\")
                                               (char=? (cadr c) #\\))))
                                    (+ i 1))))
                  (cons (string-dequote val) (+ (string-length val) 2))))
              ; FIXME: does json support single-quote strings?
              ((begins-with #(#\t #\r #\u #\e) data i)
                (cons #t 4))
              ((begins-with #(#\f #\a #\l #\s #\e) data i)
                (cons #f 5))
;             ((char=? (vector-ref data i) #\[)
;               ; FIXME: how to read array?
;               ...)
              (else
                (cons '() (- i idx))))))

    (let ((data (read-from 0)))
      ; (display (car data)) (newline)
      (car data))))

