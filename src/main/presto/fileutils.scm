(define (last-char str)
  (string-ref str (- (string-length str) 1)))

(define (ends-with-slash? pathstr)
  (char=? (last-char pathstr) #\/))

(define (begins-with-slash? pathstr)
  (char=? (string-ref pathstr 0) #\/))

(define (nodotfiles file)
  (not (eq? (string-ref file 0) #\.)))

(define (path-join basedir . extra)
  (let iter ((stack (list basedir))
             (e extra))
    (cond ((null? e)
            (join "" (reverse stack)))
          ((and (ends-with-slash? (car stack))
                (begins-with-slash? (car e)))
            (iter (cons (substring (car e) 1) stack) (cdr e)))
          ((ends-with-slash? (car stack))
            (iter (cons (car e) stack) (cdr e)))
          (else
            (iter (cons (car e) (cons "/" stack)) (cdr e))))))
