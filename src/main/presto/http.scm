; TODO:
; - guard call/cc request handling wrt exceptions as with the testsuite

(define *alog* #f)
(define *elog* #f)

(define (http-initialize)
  (set! *alog* (get-access-log-logger))
  (set! *elog* (get-error-log-logger)))

; FIXME: use some kind of combinatorial map()-function to iterate over
; outer+inner with just one indentation level
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

(define (update-handler-cache file mtime env mutex)
  (set! *handler-cache* (update-alist *handler-cache* file (list mtime env mutex))))


(define (load-handler file)
  (let ((mtime (file-modification-time file))
        (env (load-file file))
        (name (basename file))
        (mutex (make-mutex)))
    (*elog* 'info "loading handler '" name "'.")
    (if (eval '(initialize) env) ; delay initialization to get orderly logging?
        (update-handler-cache file mtime env mutex)
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
                   (env (list-ref handler 2))
                   (mutex (list-ref handler 3)))
              (mutex-lock! mutex)
              (cond ((eval `(is-handler? ,request) env)
                      (mutex-unlock! mutex)
                      file)
                    (else
                      (mutex-unlock! mutex)
                      (iter (cdr handlers)))))))))

(define (eval-handler file request)
  (let ((handler (assoc file *handler-cache*)))
    (cond ((not handler)
            (*elog* 'error "could not find handler '" file "'.")
            (list 404 '() (string->utf8 (html-error-page 404))))
          (else
            (let ((file (list-ref handler 0))
                  (mtime (list-ref handler 1))
                  (env (list-ref handler 2))
                  (mutex (list-ref handler 3)))
              (mutex-lock! mutex)
              (let ((result (eval `(get-html ,request) env)))
                (mutex-unlock! mutex)
                result))))))

(define (error-response-headers status)
  `(("Date" . ,(http/1.1-date-format (current-seconds)))
    ("Content-Type" . ("text/html" "charset=utf-8"))))

(define (http-server port headers)
  (define *sock* (make-listener-socket (get-address-info "0.0.0.0" port)))
  (set-socket-option! *sock* level/socket socket-opt/reuseaddr 1)
  (load-handlers)
  (let ((threaded? (conf-get (get-config) 'http-threads)))
    (let mainloop ()
      (cond (threaded?
              (let ((conn (accept *sock* (make-sockaddr) 64)))
                (let ((thread (make-thread
                                (lambda () (handle-connection conn headers #f)))))
                  (thread-start! thread))))
            (else
              (let ((conn (accept *sock* (make-sockaddr) 64)))
                (handle-connection conn headers #t))))
      (mainloop))))

(define (handle-connection conn headers closeconnection)
  (let keepalive ((in (open-input-file-descriptor conn))
                  (out (open-output-file-descriptor conn))
                  (reused #f))
    (define input (read-line in))
    ; FIXME: timeout (read-line) for keepalive connections, and close
    ; connections that are just hanging...
    (define body #f)
    (define status 200)
    (define status-ok #t)
    (define request-headers '())
    (define request '())
    (define request-body #f)
    (define response-headers headers)
    (define handler #f)
    (define time-before (car (get-time-of-day)))
    (define time-between #f)
    (define time-after #f)

    (if (or (eof-object? input)
            (< (length (string-split input)) 3))
        (begin
          (*elog* 'info "closing request connection.")
          (if (port-open? in) (close-input-port in))
          (if (port-open? out) (close-output-port out))
          (close-file-descriptor conn)
          )
        (let* ((requestline (string-split input))
               (method (car requestline))
               (path (url-decode (car (cdr requestline))))
               (proto (car (cdr (cdr requestline))))
               (args (url-arguments path)))

          (cond ((equal? proto "HTTP/1.1")
                  (set! request-headers (http/1.1-read-headers in))
                  (if (assoc 'host request-headers)
                      (let ((host (cdr (assoc 'host request-headers))))
                        ;(set! host (car (string-split host #\:)))
                        (set! response-headers (cons (cons "Host" host) response-headers))))))

          (let ((contlen (assoc 'content-length request-headers)))
            (cond ((and contlen (< 0 (string->number (cdr contlen))))
                    (set! request-body
                      (let ((bytes (string->number (cdr contlen))))
                        (utf8->string (read-bytevector bytes in))))
                    )))

          (set! request (make-request method path proto request-headers))
          (if request-body
              (request 'set-body! request-body))
          (rewrite-path request)

          (set! handler (find-handler request))

          (cond (handler
                  (let ((response (eval-handler handler request)))
                    (set! status (list-ref response 0))
                    (set! response-headers (append (list-ref response 1)
                                                   response-headers))
                    (set! body (list-ref response 2))))

                ((and (equal? "GET" method)
                      (file-exists? (path-join (request 'get-basedir) (request 'get-path)))
                      (file-regular? (path-join (request 'get-basedir) (request 'get-path))))

                  (let ((fileinfo (file-read (path-join (request 'get-basedir) (request 'get-path)))))
                    (set! body (cdr fileinfo))
                    (if (not (eq? '() (car fileinfo)))
                        (set! response-headers (append response-headers (car fileinfo))))))

                (else
                  (set! status 404)
                  (set! status-ok #f)
                  (if *elog* (*elog* 'error status " " input))
                  #f))

          (cond ((not status-ok)
                 (set! body (html-error-page status))
                 (set! response-headers (append response-headers (error-response-headers status)))))

          (if body
              (set! response-headers (append response-headers
                                            `(("Accept-Ranges" . "none")
                                              ("Content-Length" . ,(bytevector-length body))))))
          (set! time-between (car (get-time-of-day)))

          (show out (http/1.1-status-line status) #\return #\newline)
          (apply show out (format-headers response-headers))
          (write-bytevector body out) ; maybe dump with-input-from-file instead
          (flush-output-port out)
          (set! time-after (car (get-time-of-day)))

          (let ((secs (- (timeval-seconds time-after) (timeval-seconds time-before)))
                (micros (- (timeval-microseconds time-after)
                           (timeval-microseconds time-before))))
            (if *alog* (*alog* 'info status " " input " ("
                               (truncate (+ (* secs 1000) (/ micros 1000)))
                               "ms) " (if reused "reused" "new"))))))
    (cond (closeconnection
            (if (port-open? in) (close-input-port in))
            (if (port-open? out) (close-output-port out))
            (close-file-descriptor conn))
          (else
            (keepalive in out #t)))))

