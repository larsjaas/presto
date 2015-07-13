; TODO:
; - optimize redirect
; - smart path-joiner function that avoids ///
; - generic filters in the api, and also sort function
; - human-readable file sizes
; - alternate sorting (date-sorting)
; - move styling to css file
; - use css/ul/li for table layout
; - keep header/footer static with scrolling middle
; - colorize (grey <DIR>, highlight under pointer, ++)
; - rename index.css to presto.css
; - stuff page html in reverse and flip on generation

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
  ;(display path) (newline)
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

(define (get-directory-redirect request)
  (let* ((dir (request 'get-path))
         (host (request 'get-header 'host))
         (dirlocation (if (and host dir) (string-append "//" host dir "/") "")))
    (list 301
          `(("Location" . ,dirlocation)
           ("Method" . ,(request 'get-method)))
          (html-error-page 301))))

(define (ends-with-slash? pathstr)
  (char=? (string-ref pathstr (- (string-length pathstr) 1)) #\/))

(define (get-html-index request)
  (cond ((ends-with-slash? (request 'get-path))
          (get-html-directory-listing request))
        (else
          (get-directory-redirect request))))

(define (get-html-directory-listing request)
  (let* ((basedir (get-htdocs-root))
         (dir (request 'get-path))
         (entries (sort-dir (path-join basedir dir)))
         (directories (car entries))
         (files (cadr entries))
         (page '())) ; FIXME: stuff in reversed order

    (set! page
      (list "<html>" nl
            "<head>" nl
            "<title>Directory listing: " (get-path dir) "</title>" nl
            "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/index.css\">" nl
            "</head>" nl
            "<body>" nl
            "<tt>" nl
            "<table width=\"100%\">" nl
            "<thead>" nl))

    (set! page
      (append page
              (list "<tr><td align=\"right\" width=\"10%\"></td><td>"
                    (get-path-bar (get-path dir))
                    "</td><td width=\"20%\"></td></tr>")))

    (set! page (append page (list
            "<tr height=\"3px\"></tr>"
            "<tr>"
            "<td align=\"right\"><strong>Size</strong></td>"
            "<td><strong>Name</strong></td>"
            "<td><strong>Date</strong></td></tr>" nl
            "</thead>" nl
            "<tbody>" nl)))

    (let iter ((inodes directories))
      (cond ((null? inodes) '())
            (else
              (set! page (append page
                                 (list "<tr>"
                                       "<td align=\"right\">"
                                       (index-size "&lt;DIR&gt;") "</td>"
                    "<td><a href=\"" (url-encode-string (get-path (path-join dir (car inodes)))) "\">" (car inodes) "/</a></td>"
                    "<td>" (index-time (get-file-date (path-join basedir dir (car inodes)))) "</td></tr>" nl )))
              (iter (cdr inodes)))))


    (let iter ((inodes files))
      (cond ((null? inodes) '())
            (else
              (set! page (append page
                                 (list "<tr>"
                                       "<td align=\"right\">"
                                       (index-size (get-file-size (path-join basedir dir (car inodes)))) "</td>"

                    "<td><a href=\"" (url-encode-string (car inodes)) "\">" (car inodes) "</a></td>"
                    "<td>" (index-time (get-file-date (path-join basedir dir (car inodes)))) "</td></tr>" nl)))
              (iter (cdr inodes)))))

    (set! page (append page (list
          "</tbody>" nl
          "</table>" nl
          "</tt>" nl
          "</body></html>" nl)))

    (list 200
          '(("Content-Type" . ("text/html" "charset=utf-8")))
          (string->utf8 (apply show #f page)))))

