
(define (mime-type ext)
  (cond ((equal? ".html" ext) '("text/html" "charset=utf-8"))
        ((equal? ".txt" ext) '("text/plain" "charset=utf-8"))
        ((equal? ".log" ext) '("text/plain" "charset=utf-8"))
        ((equal? ".css" ext) "text/css")
        ((equal? ".png" ext) "image/png")
        ((equal? ".jpg" ext) "image/jpeg")
        ((equal? ".pdf" ext) "application/pdf")
        ((equal? ".js" ext) "application/javascript")
        (else #f)))

