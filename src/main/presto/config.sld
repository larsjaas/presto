(define-library (presto config)
  (import (scheme base) (chibi) (chibi config))
  (export set-config! get-config
          set-index-order! get-index-order
          get-htdocs-root)
  (include "config.scm"))
