; TODO:
; - file dates
; - file sizes
; - smart path-joiner function
; - pathbar instead of "parent directory"
; - generic filters in the api
; - support %-codes and + (at http.scm level)

(define (nodotfiles file)
  (not (eq? (string-ref file 0) #\.)))

(define (sort-dir path)
  (let ((files '())
        (subdirs '())
        (entries (filter nodotfiles (directory-files path))))
    (let iter ((entry entries))
      (cond ((null? entry)
             '())
            ((and (file-regular? (string-append path "/" (car entry)))
                  (file-is-readable? (string-append path "/" (car entry))))
             (set! files (cons (car entry) files))
             (iter (cdr entry)))
            ((and (file-directory? (string-append path "/" (car entry)))
                  (file-is-readable? (string-append path "/" (car entry))))
             (set! subdirs (cons (car entry) subdirs))
             (iter (cdr entry)))
            (else (iter (cdr entry)))))
    (list (sort subdirs) (sort files))))

(define (get-html-index basedir dir)
  (let* ((entries (sort-dir (string-append basedir dir)))
         (directories (car entries))
         (files (car (cdr entries)))
         (page '()))

    (set! page
      (list "<html><head><title>Directory listing: " dir "</title></head>"
            "<body>" nl
            "<ul>" nl))

    (if (not (equal? dir "/"))
        (let ((parent "/"))
          (set! page (append page (list "<li><a href=\"" parent "\">Parent directory</a>")))))

    (let iter ((dirs directories))
      (cond ((null? dirs) '())
            (else
              (set! page (append page (list "<li>"
                    "<a href=\"" dir "/" (car dirs) "\">" (car dirs) "/</a>" nl)))
              (iter (cdr dirs)))))

    (let iter ((fils files))
      (cond ((null? fils) '())
            (else
              (set! page (append page (list "<li>"
                    "<a href=\"" dir "/" (car fils) "\">" (car fils) "</a>" nl)))
              (iter (cdr fils)))))

    (set! page (append page (list
          "</ul>" nl
          "</body></html>" nl)))

    (apply show #f page)))

