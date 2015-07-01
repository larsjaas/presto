
(define *presto-version* '(0 0 5))

(define (version-string version)
  (apply show #f
    (let iter ((vlist version))
      (cond ((eq? '() vlist) '())
            ((eq? '() (cdr vlist)) vlist)
            (else (cons (car vlist) (cons "." (iter (cdr vlist)))))))))

(define *presto-version-string* (version-string *presto-version*))

(define (presto-httpd basedir port)
  (let ((headers
          (list (cons "Server" (show #f "presto/" (version-string *presto-version*)))
                (cons "Date" (make-datestring (current-seconds)))
                (cons "Connection" "close"))))
    (display headers) (newline)
    (http-server port headers basedir)))

