#lang rosette

(require "../lib/lib.rkt")
(provide (struct-out renameTB) init-renameTB init-renameTB-copies)


(struct renameTB (valid ROBlink) #:mutable #:transparent)


(define (init-renameTB param-physical-size param-arch-size)
  (define param-physical-size-log (inexact->exact (log param-physical-size 2)))

  (renameTB
    (initFalseVec-array param-arch-size)
    (initZeroVec-array param-arch-size param-physical-size-log)
  )
)


(define (init-renameTB-copies param-physical-size param-arch-size
                              param-size)
  (build-vector
    param-size
    (lambda (ignore) (init-renameTB param-physical-size param-arch-size)))
)

