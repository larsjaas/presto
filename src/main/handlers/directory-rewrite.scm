(import (chibi)
        (chibi string)
        (chibi filesystem)
        (presto fileutils)
        (presto htmlutils))

(define (last-char str)
  (string-ref str (- (string-length str) 1)))

(define (ends-with-slash? pathstr)
  (char=? (last-char pathstr) #\/))

(define (get-directory-redirect request)
  (let* ((dir (request 'get-path))
         (host (request 'get-header 'host))
         (dirlocation (if (and host dir) (string-append "//" host dir "/") "")))
    (list 301
          `(("Location" . ,dirlocation)
           ("Method" . ,(request 'get-method)))
          (html-error-page 301))))

(define (is-handler? request)
  (let* ((basedir (request 'get-basedir))
         (path (path-join basedir (request 'get-path))))
    (and (file-directory? path)
         (not (ends-with-slash? path)))))

(define (get-html request)
  (get-directory-redirect request))
