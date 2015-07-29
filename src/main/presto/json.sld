(define-library (presto json)
  (import (chibi string)
          (scheme base)
          (scheme char)
          (presto alist)
          (presto logging))
  (export alist? repeat-count json-initialize sexp->json json->sexp
          json-prettify)
  (include "json.scm"))
