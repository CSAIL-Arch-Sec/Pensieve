#lang rosette

(require "../lib/lib.rkt" "../inst.rkt")

(provide
  init-brPred-cfg brPred-cfg-evaluate init-brPred
  brPred-predict feedBrInfo-brPred! tick-brPred!
)


; PART brPred-cfg
(struct brPred-cfg (uninterF param-brInfo-len param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (brPred-cfg->string this) port))]
)


(define (init-brPred-cfg param-brInfo-len param-simuCycle param-symType)
  (brPred-cfg
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 0 1))]

      [(equal? param-symType "func_sym")
        (build-unfuncbv (* param-simuCycle (add1 param-brInfo-len)) 1)])
    param-brInfo-len
    param-simuCycle)
)


(define (brPred-cfg-evaluate brPred-cfg-sym sol)
  (define uninterF (brPred-cfg-uninterF brPred-cfg-sym))
  (define param-brInfo-len (brPred-cfg-param-brInfo-len brPred-cfg-sym))
  (define param-simuCycle (brPred-cfg-param-simuCycle brPred-cfg-sym))

  (brPred-cfg (evaluate uninterF sol) param-brInfo-len param-simuCycle)
)


(define (brPred-cfg->string brPred-cfg)
  "brPred-cfg"
)


; PART brPred
(struct brPred (history historyValid uninterF clk)
  #:mutable #:transparent
  ; #:methods gen:custom-write
  ;   [(define (write-proc this port mode)
  ;            (write-string (brPred->string this) port))]
)


(define (init-brPred brPred-cfg)
  (define uninterF (brPred-cfg-uninterF brPred-cfg))
  (define param-brInfo-len (brPred-cfg-param-brInfo-len brPred-cfg))
  (define param-simuCycle (brPred-cfg-param-simuCycle brPred-cfg))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (brPred
    (initZeroVec-array param-simuCycle param-brInfo-len)
    (initZeroVec-array param-simuCycle 1)
    uninterF
    (bv 0 param-simuCycle-log))
)


;; NOTE: The feedBrInfo happens at the commit stage.
;        So, it will happen before the brPred-predict
(define (feedBrInfo-brPred! brPred brInfo)
  (define history (brPred-history brPred))
  (define historyValid (brPred-historyValid brPred))
  (define clk (brPred-clk brPred))

  (set-array! history clk brInfo)
  (set-array! historyValid clk (bv 1 1))
)


(define (brPred-predict brPred)
  (define uninterF (brPred-uninterF brPred))
  (define history (brPred-history brPred))
  (define historyValid (brPred-history brPred))

  (bitvector->bool (uninterF (concat (array->bv history)
                                     (array->bv historyValid))))
)


(define (tick-brPred! brPred)
  (define clk (brPred-clk brPred))
  
  (set-brPred-clk! brPred (bvadd1 clk))
)


(define (testMe)

  (define brPred-cfg (init-brPred-cfg 2 10 "func_concrete"))
  (define brPred (init-brPred brPred-cfg))
  (printf (~a brPred "\n"))

  (printf (~a (brPred-predict brPred) "\n"))
  (printf (~a (feedBrInfo-brPred! brPred (bv 3 2)) "\n"))
  (printf (~a brPred "\n"))

  (tick-brPred! brPred)
  (printf (~a "-----------------\n"))

  (printf (~a (brPred-predict brPred) "\n"))
  (printf (~a (feedBrInfo-brPred! brPred (bv 1 2)) "\n"))
  (printf (~a brPred "\n"))
)
;(testMe)

