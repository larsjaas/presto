(define-library (presto)
  (import (chibi)
          (chibi string)
          (presto version)
          (presto formatting)
          (presto http))
  (export presto-initialize
          presto-httpd
          get-presto-version
          get-presto-version-string)
  (include "presto/presto.scm"))
