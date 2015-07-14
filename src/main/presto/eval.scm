(define (make-import-environment)
  (let ((env (make-environment)))
    (%import env (current-environment) '(import) #t)
    env))

(define (load-file file)
  (let ((env (make-import-environment)))
    (load file env)
    env))


