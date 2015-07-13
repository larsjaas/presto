; FIXME: guard call/cc wrt exceptions as with the testsuite

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

(define *handler-cache* '())

(define *module-cache* '())

(define (update-cache the-cache file mtime env)
  (let ((copy '())
        (found #f))
    (let iter ((cache the-cache))
      (cond ((null? cache)
              (if (not found)
                  (set! copy (cons (list file mtime env) copy)))
              (reverse copy))
            ((equal? (car (car cache)) file)
              (set! copy (cons (list file mtime env) copy))
              (set! found #t)
              (iter (cdr cache)))
            (else
              (set! copy (car cache))
              (iter (cdr cache)))))))

(define (update-module-cache file mtime env)
  (set! *module-cache* (update-cache *module-cache* file mtime env)))

(define (update-handler-cache file mtime env)
  (set! *handler-cache* (update-cache *handler-cache* file mtime env)))

(define (make-import-environment)
  (let ((env (make-environment)))
    (%import env (current-environment) '(import) #t)
    env))


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

(define (load-handler file)
  (*elog* 'info "loading handler '" file "'.")
  (let ((env (make-import-environment))
        (mtime (file-modification-time file)))
    (load file env)
    (update-handler-cache file mtime env)))

(define (load-handlers)
  (let ((pathlist (conf-get (get-config) 'handler-path)))
    (cond ((not (null? pathlist))
            (let ((files (directory-files (car pathlist))))
              (cond ((not (null? files))
                      (let ((p (path-join (car pathlist) (car files))))
                        (if (and (file-regular? p)
                                 (string=? (car (reverse (path-split p))) ".scm"))
                            (load-handler p))))))))))

(define (find-handler request)
  (let iter ((handlers *handler-cache*))
    (cond ((null? handlers)
            #f)
          (else
            (let* ((handler (car handlers))
                   (file (list-ref handler 0))
                   (mtime (list-ref handler 1))
                   (env (list-ref handler 2)))
              (if (eval `(is-handler? ,request) env)
                  file
                  (iter (cdr handlers))))))))

(define (eval-handler file request)
  (let ((handler (assoc file *handler-cache*)))
    (cond ((not handler)
            (*elog* 'error "could not find handler '" file "'.")
            (list 404 '() (string->utf8 (html-error-page 404))))
          (else
            (let ((file (list-ref handler 0))
                  (mtime (list-ref handler 1))
                  (env (list-ref handler 2)))
              (eval `(get-html ,request) env))))))

(define (error-response-headers status)
  `(("Date" . ,(http/1.1-date-format (current-seconds)))
    ("Content-Type" . "text/html")))

(define (http-server port headers basedir)
  (define *sock* (make-listener-socket (get-address-info "loopback" port)))
  (set-socket-option! *sock* level/socket socket-opt/reuseaddr 1)
  (load-handlers)
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
      (define handler #f)
      ; FIXME: check input for #<eof>
      (let* ((requestline (string-split input))
             (method (car requestline))
             (path (url-decode (car (cdr requestline))))
             (proto (car (cdr (cdr requestline))))
             (args (url-arguments path))
             (filename (valid-filename basedir path))
             (components (if filename (path-split filename) '())))

        (if (equal? proto "HTTP/1.1")
            (set! request-headers (http/1.1-read-headers in)))
        (close-input-port in) ; FIXME: implement keep-alive
        (set! request (make-request method path proto request-headers))
        (set! handler (find-handler request))

        (cond ((and (equal? "GET" method)
                    filename
                    (file-exists? filename)
                    (file-regular? filename))
                (let ((fileinfo (file-read filename)))
                  (set! body (cdr fileinfo))
                  (if (eq? '() (car fileinfo))
                      #t
                      (set! response-headers (append response-headers (car fileinfo))))))
              ((and (equal? "GET" method) ; FIXME: separate handler
                    filename
                    (file-exists? filename)
                    (equal? (car (reverse components)) ".scm"))
                (let ((response (eval-module filename request)))
                  (set! status (car response))
                  (set! response-headers (append (cadr response) response-headers))
                  (set! body (car (cdr (cdr response))))))
              (handler
                (let ((response (eval-handler handler request)))
                  (set! status (list-ref response 0))
                  (set! response-headers (append (list-ref response 1)
                                                 response-headers))
                  (set! body (list-ref response 2))))
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

