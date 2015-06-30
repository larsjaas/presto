; (path-split "/path/to/readme.txt") => '("/path/to/" "readme" ".txt")
(define (path-split path)
  (define (path-fix elements) ; util to prefix "." to string-split filename
    (let iter ((first #t)
               (elts elements))
      (if (eq? '() elts)
          '()
          (if first
              (cons (car elts)
                    (iter #f (cdr elts)))
              (cons (string-append "." (car elts))
                    (iter #f (cdr elts)))))))
    (let ((d '())  ; directory path characters
          (f '())) ; filename characters
      (string-map
        (lambda (c)
          (cond ((eq? c #\/)
                  (set! d (cons c (append f d)))
                  (set! f '()))
                (else
                  (set! f (cons c f))))
          c)
        path)
      (append
        (if (eq? '() d)
            '(#f)
            (list (list->string (reverse d))))
        (path-fix (string-split (list->string (reverse f)) #\.)))))

(define (url-decode path)
  '("url" ("arguments" ("hello"))))

;  (let ((parts (string-split path #\?)))
;    (if (and (list? parts) (cdr parts) (string? (car (cdr parts))))
;        (let ((arguments (string-split (cdr parts) #\&)))
;  #f)

