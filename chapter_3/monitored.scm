;; Exercise 3.2

(define (monitored proc)
  (let ((monitored-proc
         (let ((calls 0))
           (lambda (x)
             (cond ((eq? x 'how-many-calls?) calls)
                   ((eq? x 'reset-calls) (begin 
                                           (set! calls 0)
                                           calls))
                   (else 
                    (begin 
                      (set! calls (+ calls 1))
                      (proc x))))))))
    monitored-proc))

(define (main)
  (let ((monitored-sqrt (monitored sqrt))
        (monitored-log (monitored log)))
    (begin
      (monitored-sqrt 10)
      (monitored-sqrt 15)
      (monitored-sqrt 24)
      (monitored-log 17)
      (display (monitored-sqrt 'how-many-calls?)) (newline)
      (display (monitored-log 'how-many-calls?)) (newline)
      (display (monitored-sqrt 'reset-calls)) (newline)
      (monitored-sqrt 99)
      (display (monitored-sqrt 'how-many-calls?)) (newline))))

(main)
