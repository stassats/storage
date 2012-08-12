(in-package :storage)

(sb-c:defknown ascii-test (simple-string) boolean
    (sb-c:movable sb-c:foldable sb-c:flushable))

