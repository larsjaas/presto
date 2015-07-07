
(define (version-string version)
  (apply show #f
    (let iter ((vlist version))
      (cond ((eq? '() vlist) '())
            ((eq? '() (cdr vlist)) vlist)
            (else (cons (car vlist) (cons "." (iter (cdr vlist)))))))))

(define (join joiner elements)
  (apply string-append
    (append `(,(car elements))
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

