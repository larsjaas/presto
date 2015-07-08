; TODO:
; - file dates
; - file sizes
; - smart path-joiner function
; - pathbar instead of "parent directory"
; - support %-codes and + (at http.scm level)
; - generic filters in the api, and also sort function

(define (nodotfiles file)
  (not (eq? (string-ref file 0) #\.)))

(define (path-join basedir . extra)
  ; FIXME: check if / is required, accept varargs
  (join "/" (append (list basedir) extra)))

(define (sort-dir path)
  (let ((files '())
        (subdirs '())
        (entries (filter nodotfiles (directory-files path))))
    (let iter ((entry entries))
      (let ((thepath (if (null? entry) '() (path-join path (car entry)))))
        (cond ((null? entry)
               '())
              ((and (file-regular? thepath) (file-is-readable? thepath))
               (set! files (cons (car entry) files))
               (iter (cdr entry)))
              ((and (file-directory? thepath) (file-is-readable? thepath))
               (set! subdirs (cons (car entry) subdirs))
               (iter (cdr entry)))
              (else (iter (cdr entry))))))
    (list (sort subdirs) (sort files))))

(define (index-time seconds)
  (let ((time (seconds->time seconds)))
    (show #f (+ 1900 (time-year time)) "-"
          (pad-02 (time-month time)) "-" (pad-02 (time-day time)) " "
          (pad-02 (time-hour time)) ":" (pad-02 (time-minute time)) ":"
          (pad-02 (time-second time)))))

(define (index-size size)
  (let iter ((num (if (string? size) size (number->string size))))
    (cond ((> 9 (string-length num))
           (iter (string-append " " num)))
          (else num))))

(define (get-file-size path)
  (let ((fsize (file-size path)))
    fsize))

(define (get-file-date path)
  (let ((mtime (file-modification-time path)))
    mtime))

(define (get-html-index basedir dir)
  (let* ((entries (sort-dir (path-join basedir dir)))
         (directories (car entries))
         (files (cadr entries))
         (page '())) ; FIXME: stuff in reversed order

    (set! page
      (list "<html><head><title>Directory listing: " dir "</title></head>"
            "<body>" nl
            "<ul>" nl))

    ; FIXME: set up a multiclickable path-bar
    (if (not (equal? dir "/"))
        (let ((parent "/"))
          (set! page (append page (list "<li><a href=\"" parent "\">Parent directory</a>")))))

    (let iter ((inodes directories))
      (cond ((null? inodes) '())
            (else
              (set! page (append page (list "<li>"
                                            (index-time (get-file-date (path-join basedir dir (car inodes)))) " "
                                            (index-size "-") " "
                    "<a href=\"" (path-join basedir dir (car inodes)) "\">" (car inodes) "/</a>" nl)))
              (iter (cdr inodes)))))

    (let iter ((inodes files))
      (cond ((null? inodes) '())
            (else
              (set! page (append page (list "<li>"
                                            (index-time (get-file-date (path-join basedir dir (car inodes)))) " "
                                            (index-size (get-file-size (path-join basedir dir (car inodes)))) " "
                    "<a href=\"" (path-join dir (car inodes)) "\">" (car inodes) "</a>" nl)))
              (iter (cdr inodes)))))

    (set! page (append page (list
          "</ul>" nl
          "</body></html>" nl)))

    (apply show #f page)))

