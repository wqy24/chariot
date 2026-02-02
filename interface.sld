(define-library (mtr interface)
 (import (scheme base))
 (export)
 (begin
  (define-record-type interface
   (interface keybindings window is-active activator)
   interface?
   (keybindings interface-keybindings)
   (window interface-window)
   (is-active interface-active? set-interface-active!)
   (activator interface-activator-key))))

