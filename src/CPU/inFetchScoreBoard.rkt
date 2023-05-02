#lang rosette

(require "../lib/lib.rkt")
(provide (struct-out inFetchScoreBoard) init-inFetchScoreBoard)


(struct inFetchScoreBoard (numProducer) #:mutable #:transparent)


(define (init-inFetchScoreBoard param-simuCycle param-arch-size)
  (define param-simuCycle2 (* 2 param-simuCycle))
  (define param-simuCycle2-log
    (inexact->exact (ceiling (log param-simuCycle2 2))))

  (inFetchScoreBoard
    (initZeroVec-array param-arch-size param-simuCycle2-log)
  )
)
