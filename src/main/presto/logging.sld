(define-library (presto logging)
  (import (chibi) (srfi 33) (chibi filesystem) (chibi show) (chibi time) (presto time))
  (export make-logger)
  (include "logging.scm"))
