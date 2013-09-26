(define rand
  (let ((initial-random-state (copy-random-state)))
    (lambda (operation)
      (cond ((eq? operation 'generate) random)
            ((eq? operation 'reset)
               (lambda (. seed)
                 (begin
                   (if (null? seed)
                     (set! *random-state* initial-random-state)
                     (set! *random-state* (seed->random-state (car seed))))
                   random)))
            (else "Ilegal Operation")))))

(define main
  (lambda ()
    (display ((rand 'generate) 100)) (newline) 
    (display ((rand 'generate) 100)) (newline) 
    (display ((rand 'generate) 100)) (newline) 
    ((rand 'reset))
    (display ((rand 'generate) 100)) (newline)  
    (display ((rand 'generate) 100)) (newline)  
    (display ((rand 'generate) 100)) (newline)))

(main)
