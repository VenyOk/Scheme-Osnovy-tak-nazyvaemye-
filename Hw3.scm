(define (derivative expr)
  (cond ((not (list? expr))
        (cond
          ((number? expr) 0)
          ((symbol? expr) 1)))
        ((null? (cdr expr)) (derivative (car expr)))
        ((and (equal? (car expr) '-) (symbol? (cadr expr))) -1)
        ((equal? (car expr) '+) `(+ ,@(map derivative (cdr expr))))
        ((equal? (car expr) '-) `(- ,@(map derivative (cdr expr))))
        ((equal? (car expr) '*) 
           (if (null? (cddr expr))
               (derivative (cadr expr))
               (begin
               (let ((f (if (null? (cdddr expr))
                                 (caddr expr)
                                 (cons '* (cddr expr)))))
                (if (number? (cadr expr))
                    `(* ,(cadr expr) ,(derivative f))
                    (if (number? f)
                        `(* ,(derivative (cadr expr)) ,f)
               `(+ (* ,(derivative (cadr expr)) ,f) (* ,(cadr expr) ,(derivative f)))))))))

        ((equal? (car expr) '/) `(/ (- (* ,(derivative (cadr expr)) ,(caddr expr)) (* ,(cadr expr)
                                    ,(derivative (caddr expr)))) (* ,(caddr expr) ,(caddr expr))))
        ((equal? (car expr) 'expt)
         (cond ((symbol? (cadr expr)) `(* ,(caddr expr) (expt ,(cadr expr) ,(- (caddr expr) 1))))
               ((number? (cadr expr)) `(* ,expr ,(derivative (caddr expr)) (log ,(cadr expr))))))
        ((equal? (car expr) 'exp) `(* (exp ,(cadr expr)) ,(derivative (cadr expr))))
        ((equal? (car expr) 'sin) `(* (cos ,(cadr expr)) ,(derivative (cadr expr))))
        ((equal? (car expr) 'cos) `(* (- (sin ,(cadr expr))) ,(derivative (cadr expr))))
        ((equal? (car expr) 'log) `(/ ,(derivative (cadr expr)) ,(cadr expr)))
        (else expr)))

(define (simplify-plus expr)
  (define (loop expression result)
    (if (null? expression)
        (cond ((null? result) 0)
              ((null? (cdr result)) (car result))
              (else (cons '+ (reverse result))))
        (let ((simple-car (simplify (car expression))))
          (cond ((equal? simple-car 0) (loop (cdr expression) result))
                (else (loop (cdr expression) (cons simple-car result)))))))
  (loop (cdr expr) '()))
(define (simplify-mul expr)
  (define (loop expression result)
    (if (null? expression)
        (cond ((null? result) 1)
              ((null? (cdr result)) (car result))
              (else (cons '* (reverse result))))
        (let ((simple-car (simplify (car expression))))
          (cond ((equal? simple-car 0) 0)
                ((equal? simple-car 1) (loop (cdr expression) result))
                (else (loop (cdr expression) (cons simple-car result)))))))
  (loop (cdr expr) '()))
    

(define (simplify expr)
  (cond ((not (list? expr)) expr)
        ((equal? (car expr) '*) (simplify-mul expr))
        ((equal? (car expr) '+) (simplify-plus expr))
        (else (let loop ((expression expr) (result '()))
                (if (null? expression)
                    (reverse result)
                    (loop (cdr expression) (cons (simplify (car expression)) result)))))))
