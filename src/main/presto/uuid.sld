(define-library (presto uuid)
  (import (scheme base)
          (scheme char)
          (chibi process)
          (chibi string))
  (export gen-uuid)
  (include "uuid.scm"))
