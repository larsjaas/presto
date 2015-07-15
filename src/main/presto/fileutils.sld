(define-library (presto fileutils)
  (import (chibi) (chibi string) (presto formatting))
  (export path-join basename)
  (include "fileutils.scm"))
