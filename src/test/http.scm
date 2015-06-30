(import (chibi)
        (chibi test))
(import (presto http))

; join
(test '"one;2;three" (join ";" '("one" "2" "three")))
(test '"a - b - c" (join " - " '("a" "b" "c")))

