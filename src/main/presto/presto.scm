
(define (get-presto-version) (presto-version))

(define (get-presto-version-string) (version-string (presto-version)))

(define (presto-initialize)
  (http-initialize))

(define (presto-httpd port)
  (let ((headers
          (list (cons "Server" (show #f "presto/" (get-presto-version-string)))
                (cons "Connection" "close"))))
    (http-server port headers)))

