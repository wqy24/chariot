#| chariot-engine.scm -- this file is part of CHARIOT: Curves, Hackability And Restriction-less Instrument Oriented Tracker
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

(import (scheme base) (scheme cxr) (scheme read) (wqy24 vlws) (chariot config) (chariot read) (chariot render) (chariot codec) (wqy24 debug))

(define module (read))

(define output-conf (cond [(assq 'output module) => cdr] [else '()]))

(define-syntax init-param
 (syntax-rules ()
  [(_ conf [item ...] body ...)
   (let [[c conf]]
    (parameterize
     [[item (cond [(assq 'item c) => cdr] [else (item)])]
       ...]
     body ...))]))

(init-param output-conf [sample-rate byte-depth big-endian signed]
 (define channels
  (let [[data (cdr (assq 'channels module))]]
   (map (lambda (d) (let ([head (append (car d) module)])
                     (cons head (get-notes (cdr d) head)))) data)))

 (let again [[command (read)] [cache-hint 0]]
  (case (car command)
   [[play]
    (let* [[start-frm (cadr command)]
           [len (caddr command)]
           [audio-stream
            (merge-channels
             (map (lambda (c) (render-channel channel cache-hint)) channels)
             (map (lambda (c) (cdr (assq 'volume (car c)))) channels))]]
     (write-bytevector
      (codec
       (stream->list
        (let [[totake (stream-drop audio-stream start-frm)]]
         (if (integer? len) (stream-take totake len) totake))))))
    (again (read) cache-hint)]
   [[tmp-set]
    (let [[p (assq (cadr command) module)]]
     (if p
      (set-cdr! p (caddr command))
      (set! module (cons (cons (cadr command) (caddr command)) module))))
    (again (read) cache-hint)]
   [[cache-hint-set] (again (read) (cadr command))]
   [[exit] 0])))
