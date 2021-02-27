(define (spec->packages spec)
  (call-with-values (lambda ()
                      (specification->package+output spec)) list))

(define packages-list
  '("lokke"))

(packages->manifest
 (map spec->packages packages-list))
