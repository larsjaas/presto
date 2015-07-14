(import (chibi)
        (chibi filesystem)
        (presto alist)
        (presto eval)
        (presto fileutils)
        (presto parse))

(define *module-cache* '())

(define (update-module-cache file mtime env)
  (set! *module-cache* (update-alist *module-cache* file mtime env)))

(define (eval-module path request)
  (let ((mtime (file-modification-time path))
        (cached (assoc path *module-cache*)))
    (cond ((or (not cached)
               (and cached (> mtime (list-ref cached 1))))
            ; update the module cache
            (let ((env (load-file path)))
              (update-module-cache path mtime env)
              (eval `(application ,request) env)))
          (else
            ; reuse the module from the cache
            (let ((env (list-ref cached 2)))
              (eval `(application ,request) env))))))

(define (is-handler? request)
  (let ((basedir (request 'get-basedir))
        (components (path-split (request 'get-path))))
    (and (file-exists? (path-join basedir (request 'get-path)))
         (string=? (car (reverse components)) ".scm"))))

(define (get-html request)
  (let* ((basedir (request 'get-basedir))
         (path (path-join basedir (request 'get-path)))
         (mtime (file-modification-time path))
         (components (path-split path)))

    (if (and (file-exists? path)
             (string=? (car (reverse components)) ".scm"))

        (let ((response (eval-module path request)))
          response))))

