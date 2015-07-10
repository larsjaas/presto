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
      (string-for-each
        (lambda (c)
          (cond ((eq? c #\/)
                  (set! d (cons c (append f d)))
                  (set! f '()))
                (else
                  (set! f (cons c f)))))
        path)
      (append
        (if (eq? '() d)
            '(#f)
            (list (list->string (reverse d))))
        (path-fix (string-split (list->string (reverse f)) #\.)))))

(define *hexdigits* "0123456789abcdef")

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
               (set! chars (cons (integer->char (+ (* 16 (string-find *hexdigits* (char-downcase (cadr mode)))) (string-find *hexdigits* (char-downcase (car mode))))) chars))
               (set! mode '()))))
      path)
    (list->string (reverse chars))))

(define (url-arguments url)
  (define (list->pair l)
    (cond ((and (pair? l) (= (length l) 2))
           (cons (car l) (cadr l)))
          (else l)))
  (let ((strings (string-split url #\? 2)))
    (if (= 2 (length strings))
      (let iter ((parts (string-split (cadr strings) #\&)))
        (cond ((null? parts) '())
              (else
                (cons (list->pair (string-split (car parts) #\= 2))
                      (iter (cdr parts))))))
      '())))
