; FIXME: guard call/cc wrt exceptions as with the testsuite

(define *alog* #f)
(define *elog* #f)

(define (http-initialize)
  (set! *alog* (get-access-log-logger))
  (set! *elog* (get-error-log-logger)))

(define (rewrite-path request)
  (let outer ((indices (conf-get (get-config) 'index-order)))
    (let inner ((roots (conf-get (get-config) 'htdocs-root)))
      (cond ((null? roots)
              (if (not (null? indices))
                  (outer (cdr indices))))
            ((null? indices)
              (request 'get-path))
            (else
              (request 'set-basedir! (car roots))
              (let ((path (path-join (car roots) (request 'get-path) (car indices))))
                (cond ((and (file-exists? path) (file-regular? path))
                        (request 'set-basedir! (car roots))
                        (request 'set-path! (path-join (request 'get-path) (car indices))))
                      ((and (file-exists? path) (file-directory? path))
                        (request 'set-basedir! (car roots))
                        (request 'set-path! (path-join (request 'get-path) (car indices))))
                      (else
                        (inner (cdr roots))))))))))

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

(define (update-handler-cache file mtime env)
  (set! *handler-cache* (update-alist *handler-cache* file mtime env)))


(define (load-handler file)
  (let ((mtime (file-modification-time file))
        (env (load-file file))
        (name (basename file)))
    (*elog* 'info "loading handler '" name "'.")
    (if (eval '(initialize) env) ; delay initialization to get orderly logging?
        (update-handler-cache file mtime env)
        (*elog* 'warning "handler '" name "' failed to initialize. skipped."))))

(define (load-handlers)
  (let pathiter ((pathlist (conf-get (get-config) 'handler-path)))
    (cond ((not (null? pathlist))
            (let fileiter ((files (directory-files (car pathlist))))
              (cond ((not (null? files))
                      (let ((p (path-join (car pathlist) (car files))))
                        (if (and (file-regular? p)
                                 (string=? (car (reverse (path-split p))) ".scm"))
                            (load-handler p)))
                      (fileiter (cdr files)))))
            (pathiter (cdr pathlist))))))

(define (find-handler request)
  (let iter ((handlers *handler-cache*))
    (cond ((null? handlers) #f)
          (else
            (let* ((handler (car handlers))
                   (file (list-ref handler 0))
                   (mtime (list-ref handler 1))
                   (env (list-ref handler 2)))
              (cond ((eval `(is-handler? ,request) env)
                      file)
                    (else
                      (iter (cdr handlers)))))))))

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

; if no rewrite of /, use directory index handler
; if rewrite of / to index.scm, use handler for scheme, not directory index
; if rewrite of / to index.html, pipe file directly, don't use handler for directory index

; rewrite handlers:
; - rewrite path to hit file
; - find handler for file
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
             (components '()))

        (if (equal? proto "HTTP/1.1")
            (set! request-headers (http/1.1-read-headers in)))
        (close-input-port in) ; FIXME: implement keep-alive
        (set! request (make-request method path proto request-headers))
        (rewrite-path request)
        (set! components (path-split (request 'get-path)))

        (set! handler (find-handler request))

        (cond (handler
                (let ((response (eval-handler handler request)))
                  (set! status (list-ref response 0))
                  (set! response-headers (append (list-ref response 1)
                                                 response-headers))
                  (set! body (list-ref response 2))))

              ((and (equal? "GET" method)
                    (file-exists? (path-join (request 'get-basedir) (request 'get-path)))
                    (file-regular? (path-join (request 'get-basedir) (request 'get-path)))
                (let ((fileinfo (file-read (path-join (request 'get-basedir) (request 'get-path)))))
                  (set! body (cdr fileinfo))
                  (if (not (eq? '() (car fileinfo)))
                      (set! response-headers (append response-headers (car fileinfo)))))))

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

      (show out (http/1.1-status-line status) #\return #\newline)
      (apply show out (format-headers response-headers))
      (write-bytevector body out) ; maybe dump with-input-from-file instead
      (close-output-port out)
      (close-file-descriptor *conn*)
      (mainloop))))

