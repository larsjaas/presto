#!/usr/bin/env TERM= chibi-scheme -r

(import (chibi)
        (chibi config)
        (chibi filesystem)
        (chibi string))
(import (srfi 18)) ; make-thread
(import (presto)
        (presto config)
        (presto logging)
        (presto formatting)
        (presto http))

(define *config* #f)

(define (update-config-settings arguments)
  (let iter ((args arguments))
    (cond ((eq? args '()))
          (else
            (if (equal? "--" (substring (car args) 0 2))
                (let* ((arg (string-split (car args) #\=))
                       (optionname (substring (car arg) 2))
                       (optionsym (string->symbol optionname))
                       (value (if (pair? (cdr arg))
                                  (join "=" (cdr arg))
                                  (car (cdr args)))))
                  (cond ((eq? #f (conf-get *config* optionsym))
                         (set! *config* (conf-set *config* optionsym value)))
                        ((null? (conf-get *config* optionsym))
                         (set! *config* (conf-set *config* optionsym value)))
                        ((integer? (conf-get *config* optionsym))
                         (set! *config* (conf-set *config* optionsym (string->number value))))
                        (else
                         (set! *config* (conf-set *config* optionsym value))))
                  (if (not (pair? (cdr arg)))
                      (iter (cdr (cdr args)))
                      (iter (cdr args)))))))))

(define (responder-task)
  (presto-httpd (conf-get *config* 'http-port)))

(define responder (make-thread responder-task))

(define (get-config-file arguments)
  (let iter ((args arguments))
    (cond ((null? args)
            #f)
          ((or (string=? (car args) "--config")
               (string=? (car args) "-c"))
            (car (cdr args)))
          (else
            (iter (cdr args))))))

(define (main arguments)
  (let ((configfile (get-config-file arguments)))
    (cond ((and configfile (not (file-exists? configfile)))
            (display "no config '")
            (display configfile)
            (display "'.\n"))
          (else
            (set! *config* (conf-load (if configfile configfile "presto.conf")))
            (update-config-settings (cddr arguments))

            (set-config! *config*)

            ; set up all pre-initialize settings, before initialize-calls fetches and
            ; starts using the values
            (if (not (null? (conf-get *config* 'access-log)))
                (let ((logger (make-logger (conf-get *config* 'access-log)))
                      (stdout (make-logger (current-output-port))))
                  (logger 'append stdout)
                  (set-access-log-logger! logger)))


            (if (not (null? (conf-get *config* 'error-log)))
                (let ((logger (make-logger (conf-get *config* 'error-log)))
                      (stderr (make-logger (current-error-port))))
                  (logger 'append stderr)
                  (set-error-log-logger! logger)))

            (presto-initialize)

            (if (conf-get *config* 'main-thread)
                (responder-task)
                (begin
                  (thread-start! responder)
                  (thread-join! responder)))))))
