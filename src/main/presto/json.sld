(define-library (presto json)
  (import (scheme base)
          (scheme char)
          (chibi string)
          (presto alist)
          (presto logging))
  (export alist? repeat-count json-initialize sexp->json json->sexp
          json-prettify)
  (include "json.scm"))
