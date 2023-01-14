(define (count x xs)
  (define (count-iter arr cnt num)
    (if (null? arr)
        cnt
        (if (equal? (car arr) num)
            (count-iter (cdr arr) (+ 1 cnt) num)
            (count-iter (cdr arr) cnt num))))
  (count-iter xs 0 x))
      
      

(define (delete pred? xs)
  (define (loop arr arr2)
    (if (not(null? arr))
        (if (not(pred? (car arr)))
            (loop (cdr arr) (append arr2 (cons (car arr) '())))
            (loop (cdr arr) arr2))
        arr2))
  (loop xs '()))


(define (iterate f x n)
  (define (loop arr num count)
    (if (> count 0)
        (loop (append arr (cons num '())) (f num) (- count 1))
        arr))
  (loop '() x n))
            

(define (intersperse e xs)
  (define (loop arr arr2)
    (if (not(null? arr))
        (if (> (length arr) 1)
            (loop (cdr arr)
                  (append arr2
                          (cons (car arr) '())
                          (cons e '())))
            (loop (cdr arr)
                  (append arr2
                          (cons (car arr) '()))))
        arr2))
  (loop xs '()))



(define (any? pred? xs)
  (define (loop arr)
    (if (not(null? arr))
        (if (pred? (car arr))
            #t
            (loop (cdr arr)))
        #f))
  (loop xs))


(define (all? pred? xs)
  (define (loop arr)
    (if (not(null? arr))
        (if (not(pred? (car arr)))
            #f
            (loop (cdr arr)))
        #t))
  (loop xs))

(define (o . functions)
  (if (null? functions)
      (lambda (x) x)
      (lambda (x) ((car functions) ((apply o (cdr functions)) x)))))
