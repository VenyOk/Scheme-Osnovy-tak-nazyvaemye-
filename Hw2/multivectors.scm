(define (make-multi-vector sizes . fill)
  (if (null? fill)
      (cons sizes (list (make-vector (apply * sizes))))
      (cons sizes (list (make-vector (apply * sizes) (car fill))))))

(define (multi-vector? m)
  (and (list? m) (list? (car m)) (vector? (cadr m))))

(define (pos sizes indices)
  (if (null? (cdr sizes))
      (car sizes)
      (+ (* (car indices) (apply * (cdr sizes))) (pos (cdr sizes) (cdr indices)))))

(define (multi-vector-ref m indices)
  (vector-ref (cadr m) (pos (car m) indices)))


(define (multi-vector-set! m indices x)
  (vector-set! (cadr m) (pos (car m) indices) x))

