; stackless actor-based web application server framework

(define *alog* #f)
(define *elog* #f)


(define (http-initialize)
  (set! *alog* (get-access-log-logger))
  (set! *elog* (get-error-log-logger)))

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

(define (status-message status)
  (cond ((eq? status 200) "HTTP/1.1 200 OK")
        ((eq? status 404) "HTTP/1.1 404 File not found")
        (else "HTTP/1.1 404 File not found")))

(define (http-server port headers basedir)
  (define *sock* (make-listener-socket (get-address-info "loopback" port)))
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
      (define request-headers headers)
      (let* ((request (string-split input))
             (method (car request))
             (path (car (cdr request)))
             (proto (car (cdr (cdr request))))
             (filename (valid-filename basedir path)))
        (cond ((and (equal? "GET" method) filename)
                (let ((fileinfo (file-read filename)))
                  (set! body (cdr fileinfo))
                  (if (eq? '() (car fileinfo))
                      #t
                      (set! request-headers (append request-headers (car fileinfo))))))
              ((and (equal? "GET" method) (equal? "/testsuite" path))
                (set! request-headers (append request-headers
                                              `(("Content-Type" . "text/plain"))))
                (set! body (string->utf8 (call-with-output-string testsuite)))
                )
              (else
                (set! status 404)
                (set! status-ok #f)
                (if *elog* (*elog* 'error status " " input))
                #f)))

      (if (and status-ok body)
          (set! request-headers (append request-headers
                                        `(("Content-Length" . ,(bytevector-length body))))))

      (if *alog* (*alog* 'info status " " input))

      (show out (status-message status) nl)
      (if status-ok
          (apply show out (format-headers request-headers)))
      (if (and status-ok body)
          (write-bytevector body out)) ; maybe dump with-input-from-file instead
      (close-output-port out)
      (close-file-descriptor *conn*)
      (mainloop))))

