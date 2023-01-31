;O(n)
(define (my-element? x xs)
  (define (loop arr)
    (and (not(null? arr))
        (or (equal? (car arr) x)
            (loop (cdr arr)))))
  (loop xs))
;O(n^2)
(define (list->set xs)
  (define (loop arr arr2)
    (if (null? arr)
        arr2
        (if (not(my-element? (car arr) arr2))
            (loop (cdr arr) (append arr2 (cons (car arr) '())))
            (loop (cdr arr) arr2))))
  (loop xs '()))


;O(n^2)
(define (set? xs)
  (and (equal? xs (list->set xs))))

;O(n^2)
(define (union xs ys)
  (list->set (append xs ys)))
;O(n^2)
(define (intersection xs ys)
  (define (loop arr res)
    (if (null? arr)
        res
        (if (and (my-element? (car arr) xs) (my-element? (car arr) ys))
            (loop (cdr arr) (append res (cons (car arr) '())))
            (loop (cdr arr) res))))
  (loop (union xs ys) '()))




;O(n^2)
(define (difference xs ys)
  (define (loop arr res)
    (if (null? arr)
        res
        (if (and (my-element? (car arr) xs) (not(my-element? (car arr) ys)))
            (loop (cdr arr) (append res (cons (car arr) '())))
            (loop (cdr arr) res))))
  (loop (union xs ys) '()))


;O(n^2)
(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))

;O(N^2)
(define (set-eq? xs ys)
  (and (and(= (length xs) (length ys)) (=(length (intersection xs ys)) (length xs)))))
