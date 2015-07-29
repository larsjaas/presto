
(define (get-presto-version) (presto-version))

(define (get-presto-version-string) (version-string (presto-version)))

(define (presto-initialize)
  (http-initialize))

(define (presto-httpd port)
  (let ((headers
          (list (cons "Server" (string-append "presto/" (get-presto-version-string)))
                (cons "Connection" "keep-alive"))))
    (http-server port headers)))

