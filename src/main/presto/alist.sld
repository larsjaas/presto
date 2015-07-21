(define-library (presto alist)
  (import (chibi))
  (export alist? update-alist patch-alist alist-unlink)
  (include "alist.scm"))
