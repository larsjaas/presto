(define-library (presto json)
  (import (scheme small) (chibi string) (presto logging))
  (export alist? json-initialize sexp->json json->sexp)
  (include "json.scm"))
