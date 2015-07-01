(define (pad-02 n)
  (if (< n 10)
      (string-append "0" (number->string n))
      (number->string n)))

(define (make-datestring seconds)
  (let* ((time (seconds->time seconds)))
    (show #f (+ 1900 (time-year time)) "-"
          (pad-02 (time-month time)) "-" (pad-02 (time-day time)) " "
          (pad-02 (time-hour time)) ":" (pad-02 (time-minute time)) ":"
          (pad-02 (time-second time)) " GMT+1")))
