(define-library (chariot fool) ; Stand for "Frequency tOOLs"
 (import (scheme base) (scheme eval))
 (export notevector->freq)
 (begin
  (define (foolish-eval sexp)
   (eval sexp '(chariot fool env)))
  (define (notevector->freq v scales)
   (let* [[scale (cdr (assv (vector-ref v 2) scales))]
          [octave-rate (foolish-eval (cdr (assq 'octave-rate scale)))]
          [C-0 33/2]
          [note-ratio (foolish-eval (cdr (assoc (vector-ref v 0) scale)))]
          [octaves (foolish-eval (vector-ref v 1))]
          [octave-align (foolish-eval (cdr (assq 'octave-align scale)))]]
    (* C-0 note-ratio (expt 2 octave-align) (expt octave-rate (- octaves octave-align)))))))
