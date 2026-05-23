(define-library (chariot settings)
 (import (scheme base))
 (export CURVE-X-EPSILON ITER-STEP)
 (begin
  (define CURVE-X-EPSILON 1.0e-6)
  (define ITER-STEP 1/44100)))
