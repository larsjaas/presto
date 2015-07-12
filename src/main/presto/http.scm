; stackless actor-based web application server framework

(define *alog* #f)
(define *elog* #f)

(define (http-initialize)
  (set! *alog* (get-access-log-logger))
  (set! *elog* (get-error-log-logger)))

(define (valid-filename basedir path)
  (let ((filename (string-append basedir path))
        (index #f))
    (if (not (and (file-exists? filename) (file-regular? filename)))
      (let iter ((indices (get-index-order)))
        (cond ((null? indices) index)
              ((not (and index (file-exists? index)))
                (set! index (string-append basedir path "/" (car indices)))
                (iter (cdr indices)))))
      (set! index filename))
    (if (and (file-exists? index)
             (file-is-readable? index))
        index
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

(define *module-cache* '())

(define (update-module-cache file mtime env)
  (let ((copy '())
        (found #f))
    (let iter ((cache *module-cache*))
      (cond ((null? cache)
              (if (not found)
                  (set! copy (cons (list file mtime env) copy)))
              (set! *module-cache* (reverse copy)))
            ((equal? (car (car cache)) file)
              (set! copy (cons (list file mtime env) copy))
              (set! found #t)
              (iter (cdr cache)))
            (else
              (set! copy (car cache))
              (iter (cdr cache)))))))

(define (make-import-environment)
  (let ((env (make-environment)))
    (%import env (current-environment) '(import) #t)
    env))

; FIXME: guard call/cc wrt exceptions as with the testsuite

(define (eval-module file request)
  (let ((mtime (file-modification-time file))
        (cached (assoc file *module-cache*)))
    (cond ((or (not cached)
               (and cached (> mtime (list-ref cached 1))))
            ; update the module cache
            (let ((env (make-import-environment)))
              (load file env)
              (update-module-cache file mtime env)
              (eval `(application ,request) env)))
          (else
            ; reuse the module from the cache
            (let ((env (list-ref cached 2)))
              (eval `(application ,request) env))))))

(define (error-response-headers status)
  `(("Date" . ,(http/1.1-date-format (current-seconds)))
    ("Content-Type" . "text/html")))

(define (http-server port headers basedir)
  (define *sock* (make-listener-socket (get-address-info "loopback" port)))
  (set-socket-option! *sock* level/socket socket-opt/reuseaddr 1)
  (let mainloop ()
    (let* ((*addr* (make-sockaddr))
           (*conn* (accept *sock* *addr* 32))
           (in (open-input-file-descriptor *conn*))
           (out (open-output-file-descriptor *conn*)))
      (define input (read-line in))
      (define body #f)
      (define status 200)
      (define status-ok #t)
      (define request-headers '())
      (define request '())
      (define response-headers headers)
      (let* ((request (string-split input))
             (method (car request))
             (path (url-decode (car (cdr request))))
             (proto (car (cdr (cdr request))))
             (args (url-arguments path))
             (filename (valid-filename basedir path))
             (components (if filename (path-split filename) '())))

        (if (equal? proto "HTTP/1.1")
            (set! request-headers (http/1.1-read-headers in)))
        (close-input-port in)
        (set! request (make-request method path proto request-headers))

        (cond ((and (equal? "GET" method)
                    filename
                    (file-exists? filename)
                    (equal? (car (reverse components)) ".scm"))
                (let ((response (eval-module filename request)))
                  (set! status (car response))
                  (set! response-headers (append (cadr response) response-headers))
                  (set! body (car (cdr (cdr response))))))
              ((and (equal? "GET" method)
                    filename
                    (file-exists? filename)
                    (file-regular? filename))
                (let ((fileinfo (file-read filename)))
                  (set! body (cdr fileinfo))
                  (if (eq? '() (car fileinfo))
                      #t
                      (set! response-headers (append response-headers (car fileinfo))))))
              ((and (equal? "GET" method) filename (file-directory? filename))
                (let ((response (get-html-index request)))
                  (set! status (car response))
                  (set! response-headers (append response-headers
                                                 (cadr response)))
                  (set! body (car (cdr (cdr response))))))
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
             (set! body (html-error-page status))
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

