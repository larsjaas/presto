; TODO:
; - move to handlers/ and trigger on /testsuite (redirect to /testsuite/?)
; - trigger on single testsuite as /testsuite/json-test.scm
; - make link for each subsuite on index
; - on subsuite-runs, maybe parse the scm text to display the tests with
;   red and green and more output

(import (scheme small)
        (srfi 1)
        (chibi)
        (chibi string)
        (chibi time)
        (chibi filesystem)
        (chibi ast))

(define (make-import-environment)
  (let ((env (make-environment)))
    (%import env (current-environment) '(import) #t)
    env))

(define failed #f)

(define (run-testsuite-untimed file)
  (set! failed #f)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (arg)
          (display " (failed)\n")
          (display arg) (newline)
          (set! failed #t)
          (k))
        (lambda ()
          (let ((env (make-import-environment)))
            (load file env))
          (gc))))))

(define (run-testsuite file)
  (display (last (string-split file #\/)))
  (display ": ")
  (let ((before (car (get-time-of-day))))
    (run-testsuite-untimed file)
    (cond ((not failed)
           (let* ((after (car (get-time-of-day)))
                  (secs (- (timeval-seconds after) (timeval-seconds before)))
                  (micros (- (timeval-microseconds after)
                             (timeval-microseconds before))))
               (display " (")
               (display (truncate (+ (* secs 1000) (/ micros 1000))))
               (display "ms)\n"))))))

(define (testsuite out)
  (let ((old-port (current-output-port)))
    (current-output-port out)
    (display "Running testsuite\n=================\n")
    (map (lambda (file) 
           (let ((filepath (string-append "src/test/" file)))
             (if (file-regular? filepath)
                 (run-testsuite filepath))))
         (directory-files "src/test/"))
    (current-output-port old-port)))

(define (initialize)
  #t)

(define (is-handler? request)
  (and (string=? "GET" (request 'get-method))
       (string=? "/testsuite" (request 'get-path))))

(define (get-html request)
  (list 200
        '(("Content-Type" . "text/plain"))
        (string->utf8 (call-with-output-string testsuite))))
