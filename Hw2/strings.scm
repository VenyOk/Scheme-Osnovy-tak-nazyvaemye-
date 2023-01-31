;O(n^2)
(define (string-trim-left s)
  (if (char-whitespace? (string-ref s 0))
      (string-trim-left (substring s 1))
      s))


;O(n^2)
 (define (string-trim-right s)
  (if (char-whitespace? (string-ref s (- (string-length s) 1)))
      (string-trim-right (substring s 0 (- (string-length s) 1)))
      s))       

;O(n^2)
(define (string-trim s)
  (string-trim-right (string-trim-left s)))

;O(n)
(define (string-prefix? a b)
  (and (>= (string-length b) (string-length a))
      (and (equal? (substring b 0 (string-length a)) a))))


;O(n)
(define (string-suffix? a b)
  (and (>= (string-length b) (string-length a))
      (and (equal? (substring b (- (string-length b) (string-length a))) a))))


;O(n^2)
(define (string-infix? a b)
  (and (>= (string-length b) (string-length a))
      (or (string-prefix? a b)
          (string-infix? a (substring b 1)))))


;O(n^2)
(define (add-elem s sep)
  (cond
    ((= (string-length s) 0) (string))
    ((string-prefix? sep s) (string))
    (else (string-append (make-string 1 (string-ref s 0)) (add-elem (substring s 1) sep)))))
;O(n^3)
(define (string-split s sep)
  (cond
    ((= (string-length s) 0) (list))
    ((string-prefix? sep s) (string-split (substring s (string-length sep)) sep))
    (else (cons (add-elem s sep) (string-split (substring s (string-length (add-elem s sep))) sep)))))
