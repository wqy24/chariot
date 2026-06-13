(import (scheme base) (scheme lazy) (scheme write) (scheme read) (srfi 64) (scheme inexact) (chariot write-sample) (scheme file) (chariot curves) (chariot fool))

(define (force-list l)
 (if (pair? l) (cons (car l) (force-list (force (cdr l)))) '()))

(write-samples! '(1 222 3 4444) (open-binary-output-file "x.raw"))

(test-group "bezier"
 (for-each (lambda (a b) (test-approximate b a (/ 44100 16))) (force-list (bezier 44100 '(0 . 0) '(0.2 . -0.3) '(0.3 . 0.4) '(0.7 . 0.1) '(0.6 . 0.7) '(1 . 1))) (read (open-input-file "curve1")))
 (for-each (lambda (a b) (test-approximate b a (/ 44100 16))) (force-list (bezier 44100 '(0 . 1) '(0.5 . 0.2) '(0.2 . 0.3) '(0.7 . 0.1) '(0.8 . -1) '(1 . 0))) (read (open-input-file "curve2")))
 (test-error "Bad curve" (bezier 3 '(0 . 1) '(0.6 . 0.2) '(0.6 . 0.4) '(0.8 . 0.3) '(0.3 . -0.8) '(1 . 0))))

(test-group "fool"
 (let [[scales '((#\p (octave-align . 3) (octave-rate . (+ 1 1)) ("C-" . 1) ("G-" . 3/2) ("F!" . (expt 2 5/12))))]]
  (test-eqv (notevector->freq #("G-" 4 #\p) scales) 364)))
