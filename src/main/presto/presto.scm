
(define *presto-version* '(0 0 5))

(define (version-string version)
  (apply show #f
    (let iter ((vlist version))
      (cond ((eq? '() vlist) '())
            ((eq? '() (cdr vlist)) vlist)
            (else (cons (car vlist) (cons "." (iter (cdr vlist)))))))))

(define *presto-version-string* (version-string *presto-version*))

(define (pad-02 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (make-datestring)
  (let* ((time (seconds->time (current-seconds))))
    (show #f (+ 1900 (time-year time)) "-"
          (pad-02 (time-month time)) "-" (pad-02 (time-day time)) " "
          (pad-02 (time-hour time)) ":" (pad-02 (time-minute time)) ":"
          (pad-02 (time-second time)) " GMT+1")))

(define (presto-httpd basedir port)
  (let ((headers
          (list (cons "Server" (show #f "presto/" (version-string *presto-version*)))
                (cons "Date" (make-datestring))
                (cons "Connection" "close"))))
    (display headers) (newline)
    (http-server port headers basedir)))

