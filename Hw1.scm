(define (day-of-week day month year)
  (if (< month 3)
      (remainder(+ day (quotient (* 31 (+ month 10)) 12) (- year 1) + (quotient(- year 1) 4) (* -1 (quotient (- year 1) 100)) (quotient ( - year 1) 400)) 7)
      (remainder(+ day (quotient (* 31 (- month 2)) 12) year (quotient year 4) (* -1 (quotient year 100)) (quotient year 400)) 7)))



(define (square-equation a b c)
  (if (< (- (* b b) (* 4 a c)) 0)
      (list)
      (if (= (- (* b b) (* 4 a c)) 0)
          (list (/ (- b) (* 2 a)))
          (if (< (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
                 (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))  
              (list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
                    (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
              (list (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))
                    (/ (+ (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))))))



(define (my-gcd a b)
  (if (= b 0)
      a
      (my-gcd b (remainder a b))))



(define (my-lcm a b)
  (/(* a b) (my-gcd a b)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))

