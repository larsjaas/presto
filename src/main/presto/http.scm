; stackless actor-based web application server framework

(define *alog* #f)
(define *elog* #f)

(define *version* "")


(define (http-initialize)
  (set! *alog* (get-access-log-logger))
  (set! *elog* (get-error-log-logger)))

(define (valid-filename basedir path)
  (let ((filename (string-append basedir path))
        (index (string-append basedir path "/index.html")))
    (if (and (file-directory? filename)
             (file-exists? index))
        (set! filename index))
    (if (and (file-exists? filename)
             (file-is-readable? filename))
        filename
        #f)))

(define (file-read pathname)
  (let* ((headers '())
         (pathelts (path-split pathname))
         (ext (if (<= 2 (length pathelts)) (car (reverse pathelts)) ""))
         (type (mime-type ext)))
    (cond ((list? type)
            (set! headers `(("Content-Type" . ,(join ";" type)) . ,headers)))
          (type
            (set! headers `(("Content-Type" . ,type) . ,headers))))
    (cons headers (file->bytevector pathname))))

(define (http/1.1-date-format seconds)
  (define (weekday-string num)
    (cond ((and (<= 0 num) (>= 8 num))
           (list-ref (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") num))
          (else #f)))
  (define (month-string num)
    (cond ((and (<= 0 num) (>= 13 num))
           (list-ref (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") num))
          (else #f)))
  (let ((time (seconds->time seconds)))
    (show #f (weekday-string (time-day-of-week time))
             ", " (pad-02 (time-day time))
             " " (month-string (time-month time))
             " " (+ 1900 (time-year time))
             " " (pad-02 (time-hour time))
             ":" (pad-02 (time-minute time))
             ":" (pad-02 (time-second time))
             " GMT")))

;  "Sun, 06 Nov 1994 08:49:37 GMT") ; FIXME

(define (http/1.1-status-message status)
  (cond ((eq? status 200) "OK")
        ((eq? status 400) "Bad request")
        ((eq? status 404) "File not found")
        (else "Unknown request")))

(define (http/1.1-status-line status)
  (show #f "HTTP/1.1 "
        (number->string status) " " (http/1.1-status-message status)))

(define (http/1.1-error-page status)
  (string->utf8
    (show #f "<html>" nl
          "<head><title>" status " " (http/1.1-status-message status) "</title></head>" nl
          "<body bgcolor=\"white\">" nl
          "<center><h1>" status " " (http/1.1-status-message status) "</h1></center>" nl
          "<hr><center>presto/" *version* "</center>" nl
          "</body>" nl
          "</html>" nl)))

(define (error-response-headers status)
  `(("Date" . ,(http/1.1-date-format (current-seconds)))
    ("Content-Type" . "text/html")))

(define (http-server port headers basedir)
  (define *sock* (make-listener-socket (get-address-info "loopback" port)))
  (set! *version* (car (reverse (string-split (cdr (assoc "Server" headers)) #\/))))
  (set-socket-option! *sock* level/socket socket-opt/reuseaddr 1)
  (let mainloop ()
    (let* ((*addr* (make-sockaddr))
           (*conn* (accept *sock* *addr* 32))
           (in (open-input-file-descriptor *conn*))
           (out (open-output-file-descriptor *conn*)))
      (define input (read-line in))
      ; FIXME: read request headers before closing
      (close-input-port in)
      (define body #f)
      (define status 200)
      (define status-ok #t)
      (define response-headers headers)
      (let* ((request (string-split input))
             (method (car request))
             (path (car (cdr request)))
             (proto (car (cdr (cdr request))))
             (filename (valid-filename basedir path)))
        (cond ((and (equal? "GET" method) filename (file-regular? filename))
                (let ((fileinfo (file-read filename)))
                  (set! body (cdr fileinfo))
                  (if (eq? '() (car fileinfo))
                      #t
                      (set! response-headers (append response-headers (car fileinfo))))))
              ((and (equal? "GET" method) filename (file-directory? filename))
                (set! response-headers (append response-headers
                                              `(("Content-Type" . "text/html"))))
                (set! body (string->utf8 (get-html-index basedir path))))
              ((and (equal? "GET" method) (equal? "/testsuite" path))
                (set! response-headers (append response-headers
                                              `(("Content-Type" . "text/plain"))))
                (set! body (string->utf8 (call-with-output-string testsuite)))
                )
              (else
                (set! status 404)
                (set! status-ok #f)
                (if *elog* (*elog* 'error status " " input))
                #f)))

      (cond ((not status-ok)
             (set! body (http/1.1-error-page status))
             (set! response-headers (append response-headers (error-response-headers status)))))

      (if body
          (set! response-headers (append response-headers
                                        `(("Content-Length" . ,(bytevector-length body))))))

      (if *alog* (*alog* 'info status " " input))

      (show out (http/1.1-status-line status) nl)
      (apply show out (format-headers response-headers))
      (write-bytevector body out) ; maybe dump with-input-from-file instead
      (close-output-port out)
      (close-file-descriptor *conn*)
      (mainloop))))

