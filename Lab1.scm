(define (my-odd? n)
  (and (= 1 (remainder n 2))))

(define (my-even? n)
  (and (zero? (remainder n 2))))

(define (power b e)
  (if (zero? e)
      1
      (* b (power b (- e 1)))))
