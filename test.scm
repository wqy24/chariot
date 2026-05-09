(import (scheme base) (scheme inexact) (chariot write-sample) (scheme file))

(write-samples! '(1 2 3 4444) (open-binary-output-file "x.raw"))
