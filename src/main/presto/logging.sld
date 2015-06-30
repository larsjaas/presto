(define-library (presto logging)
  (import (chibi) (chibi filesystem) (chibi show))
  (export make-logger)
  (include "logging.scm"))
