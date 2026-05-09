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
   (define (cbrt n)
    (if (negative? n)
     (- (expt (- n) (/ 1 3)))
     (expt n (/ 1 3))))
   (let* [[a (+ (* p5 20)
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
                (* p0 20))]
          [A (- (* b b) (* 3 a c))]
          [B (- (* b c) (* 9 a d))]
          [C (- (* c c) (* 3 b d))]
          [delta (- (* B B) (* 4 A C))]]
    (cond
     [(= A B 0) (vector (/ c b -1))]
     [(positive? delta)
      (let-values
        [[[Y1 Y2]
          (let [[Ab (* A b)] [sqdelta (sqrt delta)]]
           (values (+ Ab (* 1.5 a (- sqdelta B)))
                   (+ Ab (* -1.5 a (+ sqdelta B)))))]]
       (vector (/ (+ b (cbrt Y1) (cbrt Y2)) a -3)))]
     [(zero? delta)
      (let [[K (/ B A)]]
       (vector (- K (/ b a)) (/ K -2)))]
     [(negative? delta)
      (let* [[sqA (sqrt A)]
             [theta (acos (/ (- (* 2 A b) (* 3 a B))
                             2 (expt A (/ 3 2))))]
             [theta/3 (/ theta 3)]
             [sq3*sin_theta/3 (* (sqrt 3) (sin theta/3))]
             [cos_theta/3 (cos theta/3)]]
       (vector
        (/ (- (* -2 sqA cos_theta/3) b) 3 a)
        (/ (- (* sqA (+ cos_theta/3 sq3*sin_theta/3)) b) 3 a)
        (/ (- (* sqA (- cos_theta/3 sq3*sin_theta/3)) b) 3 a)))])))
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
       (let* [[deriv1 (curvepoint-deriv1 p0 p1 p2 p3 p4 p5)]]
        (call/cc
         (lambda (exit)
          (vector-fold
           (lambda (sig x)
            (cond
             [(< 0 x 1)
              (define dr (deriv x))
              (if (and (not (zero? sig)) (or (zero? sig) (= sig (sign dr))))
               (sign dr)
               (exit #f))]
             [else
              (define dr (if (<= x 0) (deriv1 0) (deriv1 1)))
              (if (or (and (<= sig 0) (<= dr 0)) (and (>= sig 0)))
               (sign dr)
               (exit #f))])) 0 (extremum-deriv1 p0 p1 p2 p3 p4 p5)
          #t))))) ps))))
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
