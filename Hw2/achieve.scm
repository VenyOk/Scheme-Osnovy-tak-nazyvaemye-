; Ачивка list-trim-right на 1 балл из-за квадратичной сложности
(define (count-spaces lis)
  (define (loop lis count)
    (if (null? lis)
        count
        (if (and (char? (car lis)) (char-whitespace? (car lis)))
            (loop (cdr lis) (+ count 1))
            (loop (cdr lis) 0))))
  (loop lis 0))

(define (list-trim-right lis)
  (define (loop res lis count)
    (if (zero? count)
        res
        (loop (append res (cons (car lis) '())) (cdr lis) (- count 1))))
    (loop '() lis (- (length lis) (count-spaces lis))))
