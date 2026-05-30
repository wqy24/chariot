(import (scheme base) (scheme lazy) (scheme write) (scheme inexact) (chariot write-sample) (scheme file) (chariot curves))

(define (force-list l)
 (if (pair? l) (cons (car l) (force-list (force (cdr l)))) '()))

(write-samples! '(1 222 3 4444) (open-binary-output-file "x.raw"))

(write (force-list (curve '(0 . 0) '(0.2 . -0.3) '(0.3 . 0.4) '(0.7 . 0.1) '(0.6 . 0.7) '(1 . 1) 44100)))
(write (force-list (curve '(0 . 1) '(0.5 . 0.2) '(0.2 . 0.3) '(0.7 . 0.1) '(0.8 . -1) '(1 . 0) 44100)))
