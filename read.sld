(define-library (chariot read)
 (import (scheme base) (scheme read) (chariot config) (scheme lazy) (srfi 132) (srfi 1))
 (export get-curve)
 (begin
  (define (get-head port1 port2)
   (append (read port1) (read port2)))
  (define (get-ticks port)
   (let [[sexp (read port)]]
    (cons sexp (delay (read-ticks port)))))
  (define (get-notes port head)
   (define frames/tick (truncate (* SAMPLE-RATE (assq 'tempo head))))
   (define ticks (get-ticks port)
   (let next-tick [[cticks ticks]]
    (if (eof-object? cticks)
     '()
     (let [[curr-tick (car ctiks)]]
      (let next-frame [[frames 0] [notes (list-sort curr-tick)]]
       (if (>= frames frames/tick)
        (next-tick (force (cdr cticks)))
        (let [[result (if (>= frames (* SAMPLE-RATE (caar notes))) (car notes) #f)]]
         (cons result (delay
                       (next-frame
                        (+ frames 1)
                        (if result (cdr notes) notes))))))))))))
  (define (curve-length name notes) ; in frames
   (assert notes (lambda (n) (car n)) "Notes should starts with a note start")
   (let loop [[len 1] [nnotes (force (cdr notes))]]
    (cond
     [(null? nnotes) len]
     [(not (car nnotes)) #0=(loop (+ len 1) (force (cdr nnotes)))]
     [(and (eq? name 'freq) (cadar nnotes)) len]
     [(assq name (cddar notes)) len]
     [else #0#])))
  (define (get-curve name notes head)
   )))
