(define-library (presto index)
  (import (chibi)
          (srfi 1)
          (srfi 95)
          (chibi filesystem)
          (chibi string)
          (chibi show)
          (chibi time)
          (presto formatting)
          )
  (export get-html-index)
  (include "index.scm"))