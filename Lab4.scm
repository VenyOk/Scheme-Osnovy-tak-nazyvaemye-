; Задача No 1
(define call/cc call-with-current-continuation)
(define *env* #f)
(define-syntax use-assertions
  (syntax-rules ()
    ((_)
     (call/cc (lambda (cc)
                (set! *env* cc))))))
(define-syntax assert
  (syntax-rules ()
    ((assert expression)
     (and (not expression)
          (begin
            (display "FAILED: ")
            (*env* (write 'expression)))))))
(use-assertions) ; Инициализация вашего каркаса перед использованием

; Определение процедуры, требующей верификации переданного ей значения:

(define (1/x x)
  (assert (not (zero? x))) ; Утверждение: x ДОЛЖЕН БЫТЬ ≠ 0
  (/ 1 x))

; Задача No 2
(define (load-data file)
  (call-with-input-file file
    (lambda (port) (read port))))

(define (save-data data file)
  (call-with-output-file file (lambda (port) (write data port))))

(define (file->char_list path)
  (call-with-input-file path
    (lambda (input-port)
      (let loop ((x (read-char input-port)))
        (cond 
          ((eof-object? x) '())
          ((begin (cons x (loop (read-char input-port))))))))))

(define (count-line file)
  (define str1 "")
  (define str2 "")
  (define (loop arr count)
    (if (> (length arr) 0)
        (begin (set! str1 str2)
               (set! str2 (car arr)))
        (set! str2 ""))
    (if (equal? str2 "")
        count
        (if (or (and (equal? str2 #\newline) (not (equal? str1 #\newline)) 
        (not (equal? str1 #\return))) (and (equal? str2 #\return) 
        (not (equal? str1 #\newline))))
            (loop (cdr arr) (+ count 1))
            (loop (cdr arr) count))))
  (loop (file->char_list file) 0))

; Задача No 3
(define (trib n)
  (let ((m (make-vector (+ n 1))))
    (let loop ((num n))
      (cond
        ((< num 2) 0)
        ((= num 2) 1)
        ((if (= (vector-ref m num) 0)
             (vector-set! m num
                          (+ (loop (- num 3))
                             (loop (- num 2))
                             (loop (- num 1)))))
         (vector-ref m num))))))

; Задача No 4
(define-syntax my-if
  (syntax-rules ()
    ((my-if res expr1 expr2)
     (force (or (and res (delay expr1)) (delay expr2))))))

; Задача No 5
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

; Задача No 6
(define-syntax when
  (syntax-rules ()
    ((when cond? expr ...)
     (if cond? (begin expr ...)))))
(define-syntax unless
  (syntax-rules ()
    ((when cond? expr ...)
     (if (not cond?) (begin expr ...)))))
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs expr ...)
     (let loop ((lis xs))
       (if (not (null? lis))
           (let ((x (car lis)))
             expr ...
             (loop (cdr lis))))))
    ((for xs as x expr ...)
     (for x in xs expr ...))))
(define-syntax while
  (syntax-rules ()
    ((while cond? expr ...)
     (let loop ()
       (when cond? expr ...
         (loop))))))
(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr ...) until cond?)
     (let loop ()
       expr ...
       (if (not cond?) (loop))))))
(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << expr1)
     (display expr1))
    ((cout << endl . expr)
     (begin
       (newline)
       (cout . expr)))
    ((cout << expr1 . expr)
     (begin
       (display expr1)
       (cout . expr)))))
