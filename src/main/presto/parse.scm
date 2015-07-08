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

; url-decode +=>" ", %XX=\xXX
(define (url-decode path)
  (let ((mode '())
        (chars '()))
    (string-for-each
      (lambda (c)
        (cond ((not (null? mode))
               (if (not (eq? (length mode) 3))
                   (set! mode (cons c mode))))
              ((eq? c #\%)
               (set! mode '(#\%)))
              ((eq? c #\+)
               (set! chars (cons #\space chars)))
              (else
               (set! chars (cons c chars))))
        (cond ((and (not (null? mode)) (= (length mode) 3))
               (set! chars (cons #\space chars))
               (set! mode '()))))
      path)
    (list->string (reverse chars))))

