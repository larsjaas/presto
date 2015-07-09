(define-library (presto formatting)
  (import (srfi 38) (chibi) (chibi show) (chibi string))
  (export join version-string format-headers pad-02 url-encode-string)
  (include "formatting.scm"))
