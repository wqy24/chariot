(define-library (chariot read)
 (import (scheme base) (scheme read) (chariot config) (scheme lazy) (srfi 132) (srfi 1) (srfi 133))
 (export get-curve)
 (begin
  (define-record-type curve-record
   (curve-record porc v1 c1 v2 c2 r)
   curve-record?
   [proc processer]
   [v1 value1]
   [c1 curve1]
   [v2 value2]
   [c2 curve2]
   [r repeats])

  (define (get-curve-record desc proc)
   (let loop [[d desc] [stage 'v1] [r #f] [v1 #f] [v2 #f] [r-c1 '()] [r-c2 '()]]
    (case stage
     [['v1] (loop (cdr d) 'c1 r (car d) v2 r-c1 r-c2)]
     [['c1]
      (if (pair? (car d))
       (loop (cdr d) 'c1 r v1 v2 (cons (car d) r-c1) r-c2)
       (loop d 'v2  r v1 v2 r-c1 r-c2))]
     [['v2] (loop (cdr d) 'c2 r v1 (car d) r-c1 r-c2)]
     [['c2]
      (if (pair? (car d))
       (loop (cdr d) 'c1 r v1 v2 r-c1 (cons (car d) r-c2))
       (loop d 'r r v1 v2 r-c1 r-c2))]
     [['r]
      (if (pair? d)
       (loop #f 'end (car d) v1 v2 r-c1 r-c2)
       (loop #f 'end 1 v1 v2 r-c1 r-c2))]
     [['end]
      (curve-record proc v1 (reverse r-c1) v2 (reverse r-c2) r)]))) 

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
     [(eq? name 'freq) len]
     [(assq name (cddar notes)) len]
     [else #0#])))

  (define (expt-proc higher lower v)
   (* lower (expt (- higher lower) v)))

  (define (curve-record->curve record len)
   (define lens
    (let loop [[c-lens '()] [rlen len] [c-rep (- (repeats record) 1)]]
     (if (zero? c-rep)
      (cons rlen c-lens)
      (let [[clen (truncate (/ len (repeats record)))]]
       (loop (cons (clen c-lens)) (- rlen clen) (- c-rep 1))))))
   (let loop [[lens lens]
              [curver curve1]
              [c-curve #f]]
    (cond
     [(null? lens) '()]
     [(not c-curve)
      (loop lens curver (apply bezier (cons (car lens) (curver record))))]
     [(null? c-curve)
      (loop
       (cdr lens)
       (if (eq? curver curve1)
        curve2 curve1)
       #f)]
     [else
      (cons (car c-curve) (delay (loop lens curver (force (cdr c-curve)))))]))

  (define (curve desc proc len)
   (curve-record->curve (get-curve-record desc proc) len))

  (define (get-curve name notes head)
   (let loop [[c-notes notes] [c-curve '()]]
    (cond
     [(null? c-notes) '()]
     [(pair? c-curve) (loop (force (cdr c-notes)) (force (cdr c-curve)))]
     [(not (car c-notes)) #f]
     [(eq? name 'freq)
      (loop
       c-notes
       (let [[tone (cadar c-notes)]]
        (cond
         [(vector? tone) (constant-line (curve-length 'freq c-notes) (notevector->freq tone))]
         [(pair? tone)
          (curve
           (map (lambda (x) (if (vector? x) (notevector->freq x) x)) tone)
           expt-proc
           (curve-length c-notes))])))]
     [else
      (let flagpair [[(assq name notes)]]
       (if flagpair
        (loop
         c-notes
         (cond
          [(pair? (cdr flagpair)) (curve (cdr flagpair) linear-proc (curve-length c-notes))]
          [(number? (cdr flagpair)) (constant-line (curve-length c-notes) (cdr flagpair)))]
          [else (cons (cdr flagpair) (delay '()))])
       #f))])))))
