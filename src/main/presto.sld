(define-library (presto)

  (import (chibi)
          (chibi show)
          (chibi time)
          (chibi filesystem)
          (presto http))

  (export *presto-version*
          *presto-version-string*
          presto-httpd
          )

  (include "presto/presto.scm"))
