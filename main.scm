; This is gambit scheme code using Gambit 93f8107

(import (scheme base) (scheme inexact) (wqy24 assert) #;(srfi 133) (vectr bridge))

(define-record-type event
 (event freq wavegen volume envelope state)
 event?
 [freq event-freq]
 [wavegen event-wavegen]
 [volume event-volume]
 [envelope event-envelope]
 [state event-state])

(define-record-type channel
 (channel freq current-offset wavegen volume envelope state)
 channel?
 [freq channel-freq set-channel-freq!]
 [current-offset channel-offset set-channel-offset!]
 [wavegen channel-wavegen set-channel-wavegen!]
 [volume channel-volume set-channel-volume!]
 [envelope channel-envelope set-channel-envelope!]
 [state channel-state set-channel-state!])

(define pi (acos -1))
(define e (exp 1))

(define (sinewave freq offset)
 (sin (* 2 pi freq offset)))

(define (value-of n ch)
 (cond
  [(number? n) n]
  [(procedure? n) (n ch)]
  [else (error "Bad value" n)]))

(define (channel->sample ch)
 ((channel-envelope ch) (channel-offset ch) (channel-state ch) ((channel-wavegen ch) (value-of (channel-freq ch) ch) (channel-offset ch))))

(define (apply-event! ch evt)
 (assert ch channel? "Not a channel")
 (assert evt event? "Not an event")
 (cond
  [(event-wavegen evt)
   (set-channel-wavegen! ch (event-wavegen evt))
   (set-channel-offset! ch 0)
   (set-channel-envelope! ch (event-envelope evt))])
 (let [[evt-freq (event-freq evt)]]
  (cond
   [(not evt-freq)]
   [(number? evt-freq)
    (set-channel-freq! ch evt-freq)]
   [(procedure? evt-freq)
    (set-channel-freq! ch (evt-freq ch))]
   [else (error "Wrong type for event-freq" evt-freq)]))
 (let [[evt-vol (event-volume evt)]]
  (cond
   [(not evt-vol)]
   [(number? evt-vol)
    (set-channel-volume! ch evt-vol)]
   [(procedure? evt-vol)
    (set-channel-volume! ch (evt-vol ch))]
   [else (error "Wrong type for event-volume" evt-vol)]))
 (when (event-state evt)
  (set-channel-state! ch (event-state evt))))


(define (get-sample offset)
 (if (pair? rows)
  (let [[row (car rows)]]
   (if (vector? row)
    (begin
     (unless counter
      (init-counter! tick-frames)
      (vector-for-each apply-event! channels row))
     (when (< (counter) 0)
      (init-counter! tick-frames)
      (set! rows (cdr rows))
      (vector-for-each apply-event! channels row))
     (let* [[channel-volumes  (/ 1 (vector-length channels))]]
      (vector-fold + 0
       (vector-map (lambda (ch)
                    (let* [[addup-offset (+ (channel-offset ch) offset)]
                           [current-offset (- addup-offset (* 200 (truncate (/ addup-offset 200))))]]
                     (set-channel-offset! ch current-offset)
                     (* (value-of (channel-volume ch) ch) (channel->sample ch) channel-volumes))) channels))))
    (begin
     (make-thread add-notes "add notes")
     (set! rows (cdr rows))
     (get-sample offset))))
  (begin
   (stop)
   0.0)))

(define (init-channels! count)
 (set! channels (vector-map (lambda (x) (channel 0 0 #f 0 #f #f)) (make-vector count))))

(set-sample-getter! get-sample)

(define tick-frames 20000)
(define counter #f)
(define (init-counter! tick-size)
 (set! counter
  (lambda ()
   (set! tick-size (- tick-size 1))
   tick-size)))

(define channels #f)
(define rows (list (vector (event (lambda (n) 440) sinewave (lambda (n) (lambda (x) 1)) (lambda (offset state x) x) 'sustain))))

(init-channels! 1)
(play)
