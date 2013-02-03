(define (a-plus-abs-b a b)
  ((if (> b 0)
       +
       -) a b))

;; The expression will return an operator base on the sign of the second
;; input parameter. Then the operator is applied to the two values.
   
