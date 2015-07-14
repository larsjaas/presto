(define (alist? obj)
  (let iter ((l obj))
    (cond ((null? l) #t)
          ((or (not (pair? l)) (not (pair? (car l)))) #f)
          (else
            (iter (cdr l))))))

(define (update-alist alist key . rest)
  (let iter ((cache alist) (copy '()) (found #f))
    (cond ((null? cache)
            (if found
                (reverse copy)
                (reverse (cons (cons key rest) copy))))
          ((equal? (car (car cache)) key)
            (iter (cdr cache) (cons (cons key rest) copy) #t))
          (else
            (iter (cdr cache) (cons (car cache) copy) found)))))
