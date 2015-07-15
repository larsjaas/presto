(define-library (presto uuid)
  (import (scheme small) (scheme char) (chibi process))
  (export gen-uuid)
  (include "uuid.scm"))
