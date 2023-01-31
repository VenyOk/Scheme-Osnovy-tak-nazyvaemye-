;Ачивка на +1 балл
(define (my-fold-left op xs)
  (if (> (length xs) 1)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))
      (car xs)))



(define (o . func)
  (lambda (x)
    (my-fold-left (lambda (x op) (op x)) (cons x (reverse func)))))
