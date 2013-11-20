(define (tagged-list? expression tag)
  (if (pair? expression)
      (eq? (car expression) tag)
      #f))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
       (define (lookup key-1 key-2)
         (let ((subtable (assoc key-1 (cdr local-table))))
           (if subtable
               (let ((record (assoc key-2 (cdr subtable))))
                 (if record
                     (cdr record)
                     #f))
               #f)))
       (define (insert! key-1 key-2 value)
         (let ((subtable (assoc key-1 (cdr local-table))))
           (if subtable
               (let ((record (assoc key-2 (cdr subtable))))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! subtable
                               (cons (cons key-2 value)
                                     (cdr subtable)))))
               (set-cdr! local-table
                         (cons (list key-1
                                     (cons key-2 value))
                               (cdr local-table)))))
         'ok)

       (define (dispatch m)
         (cond ((eq? m 'lookup-proc) lookup)
               ((eq? m 'insert-proc!) insert!)
               (else (error "type error" m))))
       dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each display-line stream))

(define (display-line line)
  (newline)
  (display line))

(define (cons-stream first rest)
  (cons first (delay rest)))

(define stream-null? null?)
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

(define the-empty-stream '())

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define prompt-for-input display)

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((query (query-syntax-process (read))))
    (cond ((assertion-to-be-added? query)
           (add-rule-to-assertion! (add-assertion-body query))
           (newline) (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline) (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   query
                   frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval query (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instanciate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'and 'qeval conjoin)
(put 'and 'qeval disjoin)
(put 'and 'qeval negate)
(put 'and 'qeval lisp-value)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pettern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stram unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               #t
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     #f))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))


(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((stream (get key1 key2)))
    (if stream stream
        the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (getl-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
        (put key
             'assertion-stream
             (cons-stream assertion
                          current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pattern)
  (or (constant-symbol? (car pattern))
      (var? (car pattern))))

(define (index-key-of pattern)
  (let ((key (car pattern)))
    (if (var? key) '?
        key)))

(define (use-index? pattern)
  (constant-symbol? (car pattern)))

(define (stream-append first second)
  (if (stream-null? first)
      second
      (cons-stream (stream-car first)
                   (stream-append (stream-cdr first) second))))

(define (stream-append-delayed stream delayed-stream)
  (if (stream-null? stream)
      (force delayed-stream)
      (cons-stream (stream-car stream)
                   (stream-append-delayed (stream-cdr stream)
                                          delayed-stream))))


(define (interleave-delayed stream delayed-stream)
  (if (stream-null? stream)
      (force delayed-stream)
      (cons-stream (stream-car stream)
                   (interleave-delayed (force delayed-stream)
                                       (delay (stream-cdr stream))))))


(define (stream-map procedure stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (procedure (stream-car stream))
                   (stream-map procedure (stream-cdr stream)))))


(define (stream-flatmap procedure stream)
  (flatten-stream (stream-map procedure stream)))


(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (delay
                            (flatten-stream (stream-cdr stream))))))


(define (singleton-stream x)
  (cons-stream x the-empty-stream))


(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))


(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))


(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))
(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols procedure exp)
  (cond ((pair? exp)
         (cons (map-over-symbols procedure (car exp))
               (map-over-symbols procedure (cdr exp))))
        ((symbol? exp) (procedure exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable)) "-"
                                     (number->string (cadr  variable)))
                      (symbol->string (cadr variable))))))


(query-driver-loop)
