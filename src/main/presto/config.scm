(define *config* '())
(define *index-order* '("index.html" "."))

(define (set-config! config)
  (set! *config* config)
  (if (not (null? (conf-get *config* 'index-order)))
      (set! *index-order* (conf-get *config* 'index-order)))
  )

(define (get-config)
  *config*)

(define (set-index-order! order)
  (set! *index-order* order))

(define (get-index-order)
  *index-order*)

(define (get-htdocs-root)
  (conf-get *config* 'htdocs-root))

