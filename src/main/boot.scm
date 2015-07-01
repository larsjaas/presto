#! /usr/bin/env chibi-scheme -r

(import (chibi)
        (chibi string)
        (chibi config))
(import (srfi 18)) ; make-thread
(import (presto)
        (presto parse)
        (presto logging)
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
                       (value (join "=" (cdr arg))))
                  (cond ((null? (conf-get *config* optionsym))
                         (set! *config* (conf-set *config* optionsym value)))
                        ((integer? (conf-get *config* optionsym))
                         (set! *config* (conf-set *config* optionsym (string->number value))))
                        (else
                         (set! *config* (conf-set *config* optionsym value))))
                  (iter (cdr args))))))))

(define responder
  (make-thread
    (lambda ()
      (presto-httpd (conf-get *config* 'htdocs-root)
                    (conf-get *config* 'http-port)))))

(define (main arguments)
  (set! *config* (conf-load "presto.conf"))
  (update-config-settings (cddr arguments))

  (if (not (null? (conf-get *config* 'access-log)))
      (let ((logger (make-logger (conf-get *config* 'access-log))))
        (http-set-access-log! logger)))

  (if (not (null? (conf-get *config* 'error-log)))
      (let ((logger (make-logger (conf-get *config* 'error-log))))
        (http-set-error-log! logger)))

  (thread-start! responder)
  (thread-join! responder))
