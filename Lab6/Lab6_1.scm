(define call/cc call-with-current-continuation)

;ГРАММАТИКА
; <frac> ::= <signed-num> / <unsigned-num>
; <signed-num> ::= +<unsigned-num>| -<unsigned-num>|<unsigned-num>
;<unsigned-num> ::= NUM <num-tail>
;<num-tail> ::= NUM<num-tail>|" "
; NUM ::= "0123456789"
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

(define (right-symb stream term fault)
  (if (equal? (peek stream) term)
      (next stream)
      (fault #f)))

(define (frac stream fault)
  (signed-num stream fault)
  (right-symb stream #\/ fault)
  (unsigned-num stream fault))

(define (signed-num stream fault)
  (cond
    ((or (equal? (peek stream) #\+) (equal? (peek stream) #\-))
     (next stream)
     (unsigned-num stream fault))
    (else (unsigned-num stream fault))))

(define (unsigned-num stream fault)
  (cond
    ((and (char? (peek stream)) (char-numeric? (peek stream)))
     (next stream)
     (num-tail stream fault))
    (else (fault #f))))

(define (num-tail stream fault)
  (cond
    ((and (char? (peek stream)) (char-numeric? (peek stream)))
     (next stream)
     (num-tail stream fault))
    (else #t)))

(define (check-frac str)
  (define stream (make-stream (string->list str) 'EOF))
    (call/cc
     (lambda (fault)
       (frac stream fault)
       (equal? (peek stream) 'EOF))))
