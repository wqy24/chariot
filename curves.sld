(define-library (chariot curves)
 (import (scheme base) (scheme inexact) (srfi 133) (wqy24 assert) (wqy24 math))
 (export curve)
 (begin
  (define (curvepoint-deriv1 p0 p1 p2 p3 p4 p5)
   (let [[c4 (+ (* p5 5)
                (* p4 -25)
                (* p3 50)
                (* p2 -50)
                (* p1 25)
                (* p0 -5))]
         [c3 (+ (* p4 20)
                (* p3 -80)
                (* p2 120)
                (* p1 -80)
                (* p0 20))]
         [c2 (+ (* p3 30)
                (* p2 -90)
                (* p1 90)
                (* p0 -30))]
         [c1 (+ (* p2 20)
                (* p1 -40)
                (* p0 20))]
         [c0 (+ (* p1 5)
                (* p0 -5))]]
    (lambda (t)
     (let* [[t^2 (* t t)]
            [t^3 (* t t^2)]
            [t^4 (* t t^3)]
            [t^5 (* t t^4)]]
      (+ (* c5 t^5)
         (* c4 t^4)
         (* c3 t^3)
         (* c2 t^2)
         (* c1 t)
         p0)))))
  (define (extremum-deriv1 p0 p1 p2 p3 p4 p5) ; (zero? x) of deriv2, uses Shengjin algorithm
   (let [[a (+ (* p5 20)
               (* p4 -100)
               (* p3 200)
               (* p2 -200)
               (* p1 100)
               (* p0 -20))]
         [b (+ (* p4 60)
               (* p3 -240)
               (* p2 360)
               (* p1 -240)
               (* p0 60))]
         [c (+ (* p3 60)
               (* p2 -180)
               (* p1 180)
               (* p0 -60))]
         [d (+ (* p2 20)
               (* p1 -40)
               (* p0 20))]]
    (call-with-values (lambda x x) (shengjin a b c d))))
  (define (curvepoint p0 p1 p2 p3 p4 p5) ; Five-order bezier curvepoint (normalized)
   (let [[c5 (+ p5
                (* p4 -5)
                (* p3 10)
                (* p2 -10)
                (* p1 5)
                (- p0))]
         [c4 (+ (* p4 5)
                (* p3 -20)
                (* p2 30)
                (* p1 -20)
                (* p0 5))]
         [c3 (+ (* p3 10)
                (* p2 -30)
                (* p1 30)
                (* p0 -10))]
         [c2 (+ (* p2 10)
                (* p1 -20)
                (* p0 10))]
         [c1 (+ (* p1 5)
                (* p0 5))]]
    (lambda (t)
     (let* [[t^2 (* t t)]
            [t^3 (* t t^2)]
            [t^4 (* t t^3)]
            [t^5 (* t t^4)]]
      (+ (* c5 t^5)
         (* c4 t^4)
         (* c3 t^3)
         (* c2 t^2)
         (* c1 t)
         p0)))))
  (define (validate . ps)
   (assert ps
    (lambda (ps)
     (apply
      (lambda (p0 p1 p2 p3 p4 p5)
       (let [[deriv1 (curvepoint-deriv1 p0 p1 p2 p3 p4 p5)]]
        (let loop [[sig 0] [to-validate (extremum-deriv1 p0 p1 p2 p3 p4 p5)]]
         (if (pair? to-validate)
          (define x (car (to-validate)))
          (if (< 0 x 1)
           (let* [[dr (deriv1 x)] [sigdr (sign dr)]]
            (if (and (not (zero? dr)) (or (zero? sig) (= sigdr sig)))
             (loop sigdr (cdr to-validate))
             #f))
           (let* [[dr (if (positive? x) (deriv1 1) (deriv1 0))]
                  [sigdr (sign dr)]]
            (if (or (and (<= sigdr 0) (<= sig 0))
                    (and (>= sigdr 0) (>= sig 0)))
             (loop sigdr (cdr to-validate))
             #f)))
          #t)))) ps))))
  (define (curve p0 p1 p2 p3 p4 p5 divisions) ; Returns a lazy list
   (validate p0 p1 p2 p3 p4 p5)
   (define step (/ 1 divisions))
   (define eps (/ step 2))
   (define dr1 (curvepoint-deriv1 p0 p1 p2 p3 p4 p5))
   (define point (curvepoint p0 p1 p2 p3 p4 p5))
   (let again [[cur step]]
    (if (<= (abs (- cur 1)) eps)
     '()
     (cons
      ()
      (delay again)))))))
