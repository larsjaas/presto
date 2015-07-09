
(define (version-string version)
  (apply show #f
    (let iter ((vlist version))
      (cond ((eq? '() vlist) '())
            ((eq? '() (cdr vlist)) vlist)
            (else (cons (car vlist) (cons "." (iter (cdr vlist)))))))))

(define (join joiner elements)
  (apply string-append
    (append (list (car elements))
            (let iter ((elts (cdr elements)))
              (cond ((null? elts) '())
                    (else
                      (append `(,joiner ,(car elts)) (iter (cdr elts)))))))))

(define (format-headers headers)
  (let iter ((headrs headers))
    (cond ((eq? headrs '()) `(,nl))
          (else (append `(,(car (car headrs)) ": " ,(cdr (car headrs)) ,nl)
                        (iter (cdr headrs)))))))

(define (pad-02 num)
  (if (> 10 num)
      (string-append "0" (number->string num))
      (number->string num)))

(define *hexchars*
  (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))

(define *validchars* "\"[]!@$^&*()_-=~:;{},.<>/\\|'")

(define (url-encode-string url)
  (let ((lst '()))
    (string-for-each 
      (lambda (c)
        (cond ((char=? c #\space)
               (set! lst (cons #\+ lst)))
              ((or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9))
                (set! lst (cons c lst)))
              ((< (string-find *validchars* c) (string-length *validchars*))
                (set! lst (cons c lst)))
              (else
                (let ((val (char->integer c)))
                  (set! lst (cons #\% lst))
                  (set! lst (cons (vector-ref *hexchars* (truncate (/ val 16))) lst))
                  (set! lst (cons (vector-ref *hexchars* (modulo val 16)) lst))))
              ))
      url)
    (list->string (reverse lst))))

