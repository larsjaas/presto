(define-library (presto htmlutils)
  (import (chibi) (chibi show) (chibi io) (presto http 1.1) (presto version) (presto formatting))
  (export html-error-page)
  (include "htmlutils.scm"))
