(define-library (presto json)
  (import (chibi) (chibi string) (presto logging))
  (export alist? json-initialize sexp->json json->sexp)
  (include "json.scm"))
