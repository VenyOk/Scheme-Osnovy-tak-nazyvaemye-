(define feature-if-else #t)
(define feature-while-loop #t)
(define feature-repeat-loop #t)

(define (search-index vec word index)
  (and (< index (vector-length vec))
       (if (equal? (vector-ref vec index) word)
           index
           (search-index vec word (+ index 1)))))

(define (interpret program stack)
  (let interpret-inside ((stack stack) (index 0) (variables '()) (return-stack '()))
    (if (= index (vector-length program))
        stack
        (let ((operation (vector-ref program index)))
          (cond
            ((number? operation) (interpret-inside (cons operation stack) (+ index 1) variables return-stack))
            ((equal? operation '+) (interpret-inside (cons (+ (cadr stack) (car stack))
                                                           (cddr stack)) (+ index 1) variables return-stack))
            ((equal? operation '-) (interpret-inside (cons (- (cadr stack) (car stack))
                                                           (cddr stack)) (+ index 1) variables return-stack))
            ((equal? operation '*) (interpret-inside (cons (* (cadr stack) (car stack))
                                                           (cddr stack)) (+ index 1) variables return-stack))
            ((equal? operation '/) (interpret-inside (cons (quotient (cadr stack) (car stack))
                                                           (cddr stack)) (+ index 1) variables return-stack))
            ((equal? operation 'mod) (interpret-inside (cons (remainder (cadr stack)
                                              (car stack)) (cddr stack)) (+ index 1) variables return-stack))
            ((equal? operation 'neg)
             (interpret-inside (cons (- (car stack)) (cdr stack)) (+ index 1) variables return-stack))
            ((equal? operation '=) (if (= (cadr stack) (car stack))
                 (interpret-inside (cons -1 (cddr stack)) (+ index 1) variables return-stack)
                 (interpret-inside (cons 0 (cddr stack)) (+ index 1) variables return-stack)))
            ((equal? operation '>) (if (> (cadr stack) (car stack))
                 (interpret-inside (cons -1 (cddr stack)) (+ index 1) variables return-stack)
                 (interpret-inside (cons 0 (cddr stack)) (+ index 1) variables return-stack)))
            ((equal? operation '<) (if (< (cadr stack) (car stack))
                 (interpret-inside (cons -1 (cddr stack)) (+ index 1) variables return-stack)
                 (interpret-inside (cons 0 (cddr stack)) (+ index 1) variables return-stack)))
            ((equal? operation 'not) (if (= (car stack) 0)
                 (interpret-inside (cons -1 (cdr stack)) (+ index 1) variables return-stack)
                (interpret-inside (cons 0 (cdr stack)) (+ index 1) variables return-stack)))
            ((equal? operation 'and) (if (and (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))
                 (interpret-inside (cons -1 (cddr stack)) (+ index 1) variables return-stack)
                (interpret-inside (cons 0 (cddr stack)) (+ index 1) variables return-stack)))
            ((equal? operation 'or) (if (or (not (equal? (car stack) 0)) (not (equal? (cadr stack) 0)))
                (interpret-inside (cons -1 (cddr stack)) (+ index 1) variables return-stack)
                (interpret-inside (cons 0 (cddr stack)) (+ index 1) variables return-stack)))
            ((equal? operation 'drop)
             (interpret-inside (cdr stack) (+ index 1) variables return-stack))
            ((equal? operation 'swap)
             (interpret-inside (cons (cadr stack) (cons (car stack) (cddr stack)))
                               (+ index 1) variables return-stack))
            ((equal? operation 'dup) (interpret-inside (cons (car stack) stack)
                                                       (+ index 1) variables return-stack))
            ((equal? operation 'over) (interpret-inside (cons (cadr stack) stack)
                                                        (+ index 1) variables return-stack))
            ((equal? operation 'rot) (interpret-inside (cons (caddr stack) (cons (cadr stack)
                          (cons (car stack) (cdddr stack)))) (+ index 1) variables return-stack))
            ((equal? operation 'depth) (interpret-inside (cons (length stack) stack)
                                                         (+ index 1) variables return-stack))
            ((equal? operation 'define) (interpret-inside stack (+ (search-index program 'end index) 1)
                (cons (list (vector-ref program (+ index 1)) (+ index 2)) variables) return-stack))
            ((or (equal? operation 'end) (equal? operation 'exit))
             (interpret-inside stack (car return-stack) variables (cdr return-stack)))
            ((equal? operation 'if) (if (search-index program 'else index)
                                        (interpret-inside (cdr stack) (if (zero? (car stack))
                                                    (+ (search-index program 'else index) 1)
                                                    (+ index 1)) variables return-stack)
                                        (interpret-inside (cdr stack) (if (zero? (car stack))
                                                    (+ (search-index program 'endif index) 1)
                                                    (+ index 1)) variables return-stack)))
                                            
            ((equal? operation 'else) (if (zero? (car stack))
                    (interpret-inside (cdr stack) (+ index 1) variables return-stack)
                   (interpret-inside stack (+ (search-index program 'endif index) 1) variables return-stack)))
            ((equal? operation 'endif) (interpret-inside stack (+ index 1) variables return-stack))
            ((equal? operation 'while) (if (zero? (car stack))
              (interpret-inside (cdr stack) (+ (search-index program 'wend index) 1) variables return-stack)
              (interpret-inside (cdr stack) (+ index 1) variables (cons index return-stack))))
            
            ((equal? operation 'wend) (interpret-inside stack (car return-stack)
                                                        variables (cdr return-stack)))
            ((equal? operation 'repeat)
             (interpret-inside stack (+ index 1) variables (cons index return-stack)))
            ((equal? operation 'until)
             (if (zero? (car stack))
                   (interpret-inside (cdr stack) (car return-stack) variables (cdr return-stack))
                   (interpret-inside (cdr stack) (+ index 1) variables (cdr return-stack))))
 
            (else (interpret-inside stack (cadr (assoc operation variables))
                                    variables (cons (+ index 1) return-stack))))))))

