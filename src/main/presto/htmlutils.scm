(define (html-error-page status)
  (string->utf8
    (show #f "<html>" nl
          "<head><title>" status " " (http/1.1-status-message status) "</title></head>" nl
          "<body bgcolor=\"white\">" nl
          "<center><h1>" status " " (http/1.1-status-message status) "</h1></center>" nl
          "<hr><center>presto/" (version-string (presto-version)) "</center>" nl
          "</body>" nl
          "</html>" nl)))
