(define-library (presto logging)
  (import (chibi)
          (srfi 18)
          (srfi 33)
          (chibi filesystem)
          (chibi show)
          (chibi time)
          (presto time))
  (export make-logger
          get-access-log-logger
          set-access-log-logger!
          get-error-log-logger
          set-error-log-logger!)
  (include "logging.scm"))
