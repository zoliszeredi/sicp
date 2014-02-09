(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'done
      (begin (proc (stream-car stream))
             (stream-for-each proc (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each (lambda (string)
		     (display (format #f "~a~%" string)))
		   stream))

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

(define (get-stream key1 key2)
  (let ((stream (get key1 key2)))
    (if stream stream
        the-empty-stream)))
