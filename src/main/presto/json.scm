
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
  (cond ((string? obj) ; fixme: quote " and \
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

(define (json->sexp json)
  '())

