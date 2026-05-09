(define-library (chariot curve)
 (import (scheme base) (srfi 1))
 (export curve stretch)
 (begin
  (define (stretch fn x y))
  (define (curve obj)
   (let [node1 (first obj)]
        [node2 (second obj)]
	[r (third obj)]
	[rest (cdddr obj)]
    (lambda (t)
     (* (fmod t (/ r 1)) r 
	(let loop [[robj obj]
		   [sum 0]
		   [index 0]]
	 )))))))
