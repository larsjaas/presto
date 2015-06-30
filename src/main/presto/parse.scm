; (tokenize "string" #\separator) => '("str1" "str2" ...)
(define (tokenize line . s)
  (let ((w '())
        (l '())
        (sep (if (eq? s '()) #\space (car s))))
    (string-map
      (lambda (c)
        (cond ((eq? c sep)
                (set! l (cons (list->string (reverse w)) l))
                (set! w '()))
              (else
                (set! w (cons c w))))
        c)
      line)
    (set! l (cons (list->string (reverse w)) l))
    (reverse l)))

; (path-split "/path/to/readme.txt") => '("/path/to/" "readme" ".txt")
(define (path-split path)
  (define (path-fix elements) ; util to prefix "." to tokenized filename
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
        (path-fix (tokenize (list->string (reverse f)) #\.)))))

(define (url-decode path)
  '("url" ("arguments" ("hello"))))

;  (let ((parts (tokenize "?" path)))
;    (if (and (list? parts) (cdr parts) (string? (car (cdr parts))))
;        (let ((arguments (tokenize "&" (cdr parts))))
;  #f)

