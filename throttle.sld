(define-library (vectr throttle)
 (import (scheme base) (scheme inexact) (srfi 1) (vectr head))
 (export throttler)
 (begin
  (define (enveloped e1 e2 y)
   (+ (* (/ (- e1 e2)) (+ y 1)) e2))
  (define (throttler obj head) #;(node1 node2 (x . y) ... repeat) 
   (lambda (x)
    (let [[f  (freq  obj)]
          [e1 (envelope1 obj)]
          [e2 (envelope2 obj)]
	  [o  (offset obj)]]
     (enveloped (e1 x) (e2 x) (cos (* 2 (acos -1) f (- x o)))))))
  (define (node1 obj)
   (car obj))
  (define (node2 obj)
   (cadr obj))
  (define (freq obj)
   (if (< (length obj) 1) 1 (vector-ref 1 obj)))
  (define (envelope1 obj)
   (if (< (length obj) 5) (lambda (x) 1) (read-func (fifth obj) head)))
  (define (envelope2 obj)
   (if (< (length obj) 6) (let [[e1 (envelope1 obj)]] (lambda (x) (- (e1 x)))) (read-func (sixth obj) head)))
  (define (offset obj)
   (if (< (length obj) 4) 0 (fourth obj)))))
