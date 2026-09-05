#| render.sld -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
 | Copyright (C) 2026 wqy24
 |
 | CHARIOT is free software: you can redistribute it and/or modify
 | it under the terms of the GNU General Public License as published by
 | the Free Software Foundation, either version 3 of the License, or
 | (at your option) any later version.
 |
 | CHARIOT is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 | GNU General Public License for more details.
 |
 | You should have received a copy of the GNU General Public License
 | along with CHARIOT. If not, see <https://www.gnu.org/licenses/>.
 |#

(define-library (chariot render)
 (import (scheme eval) (scheme base) (chariot read) (chariot config) (wqy24 vlws) (only (srfi 1) cons* fold) (wqy24 debug))
 (export render-channel merge-channels)
 (begin
  (define (render-channel channel hint)
   (define head (car channel))
   (define notes (cdr channel))
   (define-values [flags renderer]
    (if (and (assq 'flags head) (assq 'renderer head))
     (values (cdr (assq 'flags head)) (cdr (assq 'renderer head)))
     (let* [[engine (cdr (assq 'engine head))]
            [engine-desc (cdr (assq engine (cdr (assq 'inst-conf head))))]
            [config (cdr engine-desc)]]
      (define-values [flags renderer] ((eval 'renderer (environment (car engine-desc))) config))
      (set-car! channel (cons* (cons 'flags flags) (cons 'renderer renderer) head))
      (values flags renderer))))
   (define inst (cdr (assq 'inst head)))
   (define fresh-channel (renderer inst (map (lambda (f) (cons f (get-curve f notes head))) flags) (sample-rate) hint))
   (cond
    [(assq 'mods head) =>
     (lambda (m)
      (fold
       (lambda (mod data)
        ((eval 'apply (environment (cdr mod))) (get-curve (car mod) notes head) data))
       fresh-channel (cdr m)))]
    [else fresh-channel]))

  (define (merge-channels notes vols)
   (when (> (fold + 0 vols) 1)
    (error "Sum of vols more than 1" vols))
   (apply stream-map +
    (map (lambda (n v) (stream-map (lambda (x) (* x v)) n)) notes vols)))))
