(define call/cc call-with-current-continuation)
;Удаление пробелов - срамота, но засчитали на полный балл
(define (string-trim-left s)
  (if (zero? (string-length s))
      ""
  (if (char-whitespace? (string-ref s 0))
      (string-trim-left (substring s 1))
      s)))



 (define (string-trim-right s)
   (if (zero? (string-length s))
       ""
  (if (char-whitespace? (string-ref s (- (string-length s) 1)))
      (string-trim-right (substring s 0 (- (string-length s) 1)))
      s)))       


(define (string-trim s)
  (string-trim-right (string-trim-left s)))

(define (my-element? x xs)
  (define (loop arr)
    (and (not(null? arr))
        (or (equal? (car arr) x)
            (loop (cdr arr)))))
  (loop xs))

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

; Грамматика
; <expression> ::= <space><object><space><expression | <empty>
; <space> ::= " "<space>| <empty>
; <object> ::= ( | ) | + | - | * | / | ^ | <number> | <variable>
; <number> ::= <num><number-tail>
; <variable> ::= <word><variable-tail>
; <number-tail> ::= <num><number-tail>| . <number-tail> | <empty>
; <variable-tail> ::= <word><variable-tail> | <empty>


(define (tokenize str)
  (define (expression stream fault)
    (cond
      ((and (char? (peek stream)) (or (char-numeric? (peek stream))
                                      (char-alphabetic? (peek stream))
                                      (char-whitespace? (peek stream))
                                      (my-element? (peek stream) '(#\- #\+ #\* #\/ #\( #\) #\^))))
       (space stream fault)
       (let* ((obj (object stream fault)) (sp (space stream fault))
                                          (expr (expression stream fault)))
         (cons obj expr)))
       (else '())))
  (define (space stream fault)
    (cond
      ((and (char? (peek stream)) (char-whitespace? (peek stream)))
       (next stream)
       (space stream fault))
      (else '())))
  (define (object stream fault)
    (cond
      ((assoc (peek stream) '((#\+ +) (#\- -) (#\* *) (#\/ /) (#\^ ^) (#\) ")") (#\( "("))) =>
       (lambda (x)
         (next stream)
         (cadr x)))
      ((and (char? (peek stream)) (char-alphabetic? (peek stream)))
       (variable stream fault))
      ((and (char? (peek stream)) (char-numeric? (peek stream)))
       (number stream fault))
      (else (fault #f))))

  (define (number stream fault)
    (if (and (char? (peek stream)) (char-numeric? (peek stream)))
           (let* ((next (next stream))
                  (tail (number-tail stream fault)))
             (string->number (list->string (cons next tail))))
        (fault #f)))
  
  (define (number-tail stream fault)
    (if (and (char? (peek stream))
              (or (char-numeric? (peek stream))
                  (equal? (peek stream) #\e)
                  (equal? #\. (peek stream))))
        (let* ((next (next stream))
               (tail (number-tail stream fault)))
          (cons next tail))
        '()))
  
  (define (variable stream fault)
    (if (and (char? (peek stream))
             (char-alphabetic? (peek stream)))
        (let* ((next (next stream))
               (tail (variable-tail stream fault)))
          (string->symbol (list->string (cons next tail))))
        (fault #f)))
  
  (define (variable-tail stream fault)
    (if (and (char? (peek stream)) (char-alphabetic? (peek stream)))
        (let* ((next (next stream))
               (tail (variable-tail stream fault)))
          (cons next tail))
        '()))
  (define stream (make-stream (string->list (string-trim str)) "EOF"))
  (call/cc
   (lambda (fault)
     (define result (expression stream fault))
     (and (equal? (peek stream) "EOF")
          result))))
              

;Грамматика
;Expr    ::= Term Expr' .
;Expr'   ::= AddOp Term Expr' | .
;Term    ::= Factor Term' .
;Term'   ::= MulOp Factor Term' | .
;Factor  ::= Power Factor' .
;Factor' ::= PowOp Power Factor' | .
;Power   ::= value | "(" Expr ")" | unaryMinus Power .


(define (parse str)
  (define (expr stream fault)
    (let loop ((result (term stream fault)))
      (if (or (eqv? '+ (peek stream)) (eqv? '- (peek stream)))
          (loop (list result (next stream) (term stream fault)))
          result)))
  
  (define (term stream fault)
    (let loop ((result (factor stream fault)))
      (if (or (eqv? '* (peek stream)) (eqv? '/ (peek stream)))
          (let* ((next (next stream))
                 (f (factor stream fault)))
            (loop (list result next f)))
          result)))

  (define (factor stream fault)
    (let* ((pow (power stream fault))
          (fact1 (factor1 stream fault)))
      (if (null? fact1)
          pow
          (cons pow fact1))))

  (define (factor1 stream fault)
    (if (eqv? '^ (peek stream))
        (begin
          (next stream)
          (let* ((pow (power stream fault))
                (fact1 (factor1 stream fault)))
            (if (null? fact1)
                (cons '^ (list pow))
                (list '^ (cons pow fact1)))))
        '()))


  (define (power stream fault)
    (cond ((number? (peek stream)) (next stream))
          ((equal? "(" (peek stream))
           (begin
             (next stream)
             (let* ((exp (expr stream fault)))
               (if (not (equal? ")" (peek stream)))
                   (fault #f))
               (next stream)
               exp)))
          ((eqv? '- (peek stream))
             (next stream)
             (list '- (power stream fault)))
          ((symbol? (peek stream))
           (next stream))
          (else (fault #f))))
  (define stream (make-stream str "EOF"))
  (call/cc
   (lambda (fault)
     (let ((res (expr stream fault)))
       (and (equal? (peek stream) "EOF")
            res)))))

(define (tree->scheme tree)
  (cond ((not (list? tree)) tree)
        ((equal? (car tree) '-) (list '- (tree->scheme (cadr tree))))
        ((equal? (cadr tree) '^) (list 'expt (tree->scheme (car tree)) (tree->scheme (caddr tree))))
        (else (list (cadr tree) (tree->scheme (car tree)) (tree->scheme (caddr tree))))))
