(define-library (presto http 1.1)
  (import (scheme small)
          (chibi)
          (chibi io)
          (chibi show)
          (chibi string)
          (chibi time)
          (presto formatting))
  (export http/1.1-status-message
          http/1.1-status-line
          http/1.1-date-format
          http/1.1-read-headers)
  (include "1.1.scm"))
