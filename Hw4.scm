(define ie (interaction-environment))

(define (memoized-factorial n)
  (let ((memo_pairs '()))
    (define (loop res count)
      (cond
        ((= count (+ n 1)) res)
        (else
         (set! memo_pairs (cons (list count res) memo_pairs))
         (loop (* res count)
               (+ count 1)))))
    (loop 1 1)))

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (if (pair? p)
      (car p)
      p))

(define (lazy-cdr p)
  (if (pair? p)
      (force (cdr p))))

(define (lazy-head xs k)
  (if (> k 0)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))
      (list)))

(define (lazy-ref xs k)
  (if (= k 0)
      (lazy-car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))


(define (lazy-factorial n)
  (define (count_factorial)
    (let loop ((ans 1) (count 1))
      (lazy-cons (* ans count) (loop (* ans count) (+ count 1)))))
  (list-ref (lazy-head (count_factorial) n) (- n 1)))


(define (read-words)
  (let loop ((result '()) (cur_word '()))
    (let ((symb (read-char)))
      (cond
        ((eof-object? symb) (reverse result))
        ((char-whitespace? symb)
         (if (null? cur_word)
             (loop result cur_word)
             (loop (cons (list->string (reverse cur_word)) result) '())))
        (else (loop result (cons symb cur_word)))))))


(define-syntax define-data
  (syntax-rules ()
    ((define-data name types)
     (let loop ((list_types 'types))
       (if (not (null? list_types))
           (begin
             (eval `(define (,(caar list_types) . parameters)
                      (append (list '_d 'name ',(caar list_types)) parameters)) ie)
             (loop (cdr list_types)))
           (eval `(define (,(string->symbol (string-append (symbol->string 'name) "?"))
                           param)
                    (and (list? param) (not (null? param))
                         (equal? '_d (car param))
                         (equal? 'name (cadr param)))) ie))))))
(define-syntax match
  (syntax-rules ()
    ((match name ((type parameters ...) expressions))
     (apply (lambda (parameters ...) expressions) (cdddr name)))
    ((match name ((type parameters ...) expressions) other ...)
     (if (equal? 'type (caddr name))
         (apply (lambda (parameters ...) expressions) (cdddr name))
         (match name other ...)))))
                              
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((variable value)) functions)
     ((lambda (variable) functions) value))
    ((my-let ((variable value) . pairs) functions)
     ((lambda (variable) (my-let pairs functions)) value))))



(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () functions)
     (my-let () functions))
    ((my-let* ((variable value)) functions)
     ((lambda (variable) functions) value))
    ((my-let* ((variable value) . pairs) functions)
     ((lambda (variable) (my-let* pairs functions)) value))))
