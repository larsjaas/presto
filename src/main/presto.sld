(define-library (presto)
  (import (scheme base)
          (chibi)
          (chibi show)
          (chibi time)
          (chibi filesystem)
          (presto logging)
          (presto time)
          (presto http))
  (export presto-initialize
          presto-httpd
          get-presto-version
          get-presto-version-string)
  (include "presto/presto.scm"))
