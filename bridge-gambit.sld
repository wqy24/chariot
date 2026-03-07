(define-library (vectr bridge)
 (import (scheme base) (only gambit c-declare c-lambda c-define))
 (export set-sample-getter! play stop)
 (begin
  (c-declare
#<<end
#include "soundplay.c"
end
)
  (define gg (lambda (x) 0))
  (c-define (sample-getter o) (float) float "gg" ""
   (gg o))
  (define (set-sample-getter! g)
   (set! gg g))
  ((c-lambda () void "set_sample_getter(gg);"))
  (define (play)
   ((c-lambda () void "play();")))
  (define (stop)
   ((c-lambda () void "stop();")))))
