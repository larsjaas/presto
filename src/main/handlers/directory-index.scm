; TODO:
; - generic filters in the api, and also sort function
; - human-readable file sizes
; - alternate sorting (date-sorting)
; - keep header/footer static with scrolling middle?

(import (chibi)
        (srfi 1)
        (srfi 95)
        (chibi filesystem)
        (chibi io)
        (chibi string)
        (chibi show)
        (chibi time)
        (presto htmlutils)
        (presto formatting)
        (presto fileutils)
        )

(define (last-char str)
  (string-ref str (- (string-length str) 1)))

(define (ends-with-slash? pathstr)
  (char=? (last-char pathstr) #\/))

(define (nodotfiles file)
  (not (eq? (string-ref file 0) #\.)))

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
          (pad-02 (time-month time)) "-" (pad-02 (time-day time)) "&nbsp;"
          (pad-02 (time-hour time)) ":" (pad-02 (time-minute time)) ":"
          (pad-02 (time-second time)))))

(define (index-size size)
  (number->string size)) ; human-readable NN.NK, or NN.NNN.NNN formatting

(define (get-path path)
  (apply show #f
    (let iter ((elts (string-split path #\/))
               (parent "")
               (first #t))
      (cond ((null? elts) '())
            ((and (not first) (equal? (car elts) ""))
              (iter (cdr elts) parent #f))
            (else
              (cons
                (string-append parent (car elts) "/")
                (iter (cdr elts) "" #f)))))))

; add basedir which you don't link below
(define (get-path-bar path)
  (apply show #f
    (let iter ((elts (string-split path #\/))
               (parent "")
               (first #t))
      (cond ((null? elts) '())
            ((and (not first) (equal? (car elts) ""))
              (iter (cdr elts) parent #f))
            (else
              (cons
                (string-append "<a class=\"button\" href=\"" (get-path (string-append parent (car elts) "/")) "\">" (car elts) "/</a>")
                (iter (cdr elts) (string-append parent (car elts) "/") #f)))))))

(define (get-html-directory-listing request)
  (let* ((basedir (request 'get-basedir))
         (dir (request 'get-path))
         (entries (sort-dir (path-join basedir dir)))
         (directories (car entries))
         (files (cadr entries))
         (page '())) ; FIXME: stuff in reversed order

    (define (w . args)
      (set! page
        (let iter ((a args) (p page))
          (cond ((null? a) p)
                (else (iter (cdr a) (cons (car a) p)))))))

    (w "<html>" nl
       "<head>" nl
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/presto.css\">" nl
       "<title>Directory listing: " (get-path dir) "</title>" nl
       "</head>" nl)

    (w "<body>" nl
       "<tt>" nl)

    (w "<ul class=\"dirlist\">" nl)

    (w "<ul class=\"direntry pathbar\">" nl
       "<li class=\"size\">" nl
       "<li class=\"name\">" (get-path-bar (get-path dir)) nl
       "<li class=\"mdate\">" nl
       "<li class=\"adate\">" nl
       "<li class=\"cdate\">" nl
       "</ul>" nl)

    (w "<ul class=\"direntry header\">" nl
       "<li class=\"size\">Size" nl
       "<li class=\"name\">Name" nl
       "<li class=\"mdate\">Date" nl
       "<li class=\"adate\">Date" nl
       "<li class=\"cdate\">Date" nl
       "</ul>" nl)

    (w "<ul class=\"direntry spacing\">" nl
       "</ul>" nl)

    (let iter ((inodes directories))
      (cond ((not (null? inodes))
              (w "<ul class=\"direntry\">" nl
                 "<li class=\"size dir\">&lt;DIR&gt;" nl
                 "<li class=\"name\">"
                 "<a href=\"" (url-encode-string (path-join dir (car inodes)))
                 "\">" (car inodes) "/</a>" nl
                 "<li class=\"mdate\">"
                 (index-time (file-modification-time (path-join basedir dir (car inodes)))) nl)
              (w "<li class=\"adate\">"
                 (index-time (file-access-time (path-join basedir dir (car inodes)))) nl)
              (w "<li class=\"cdate\">"
                 (index-time (file-change-time (path-join basedir dir (car inodes)))) nl
                 "</ul>" nl)
              (iter (cdr inodes)))))

    (let iter ((inodes files))
      (cond ((not (null? inodes))
              (w "<ul class=\"direntry\">" nl
                 "<li class=\"size file\">"
                 (index-size (file-size (path-join basedir dir (car inodes)))) nl
                 "<li class=\"name\">"
                 "<a href=\"" (url-encode-string (car inodes)) "\">" (car inodes) "</a>" nl
                 "<li class=\"mdate\">"
                 (index-time (file-modification-time (path-join basedir dir (car inodes)))) nl
                 "<li class=\"adate\">"
                 (index-time (file-access-time (path-join basedir dir (car inodes)))) nl
                 "<li class=\"cdate\">"
                 (index-time (file-change-time (path-join basedir dir (car inodes)))) nl
                 "</ul>" nl)
              (iter (cdr inodes)))))

    (w "</ul>" nl)
    (w "</tt>" nl
       "</body>" nl
       "</html>" nl)

    (list 200
          '(("Content-Type" . ("text/html" "charset=utf-8")))
          (string->utf8 (apply show #f (reverse page))))))

(define (is-handler? request)
  (let* ((basedir (request 'get-basedir))
         (path (path-join basedir (request 'get-path))))
    (and (file-directory? path)
         (ends-with-slash? path))))

(define (get-html request)
  (get-html-directory-listing request))
