
(define *presto-version* '(0 0 5))

(define *access-logger* #f)

(define *error-logger* #f)

(define (version-string version)
  (apply show #f
    (let iter ((vlist version))
      (cond ((eq? '() vlist) '())
            ((eq? '() (cdr vlist)) vlist)
            (else (cons (car vlist) (cons "." (iter (cdr vlist)))))))))


(define (get-presto-version) *presto-version*)

(define (get-presto-version-string) (version-string *presto-version*))

(define (presto-httpd basedir port)
  (let ((headers
          (list (cons "Server" (show #f "presto/" (get-presto-version-string)))
                (cons "Connection" "close"))))
    (http-server port headers basedir)))

