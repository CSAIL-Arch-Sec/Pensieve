#lang rosette

(require
  "../lib/lib.rkt"
  "../abs-module/absArbiter.rkt" "../abs-module/absFifo.rkt"
  "../abs-module/absBufferGM.rkt" "../abs-module/absDelay.rkt"
)
(provide
  init-alu-cfg alu-cfg-evaluate
  init-alu alu-dataoutValid dataout-alu! datain-alu! alu-datainReady squash-alu!
  squashPartial-alu! tick-alu! alu-cfg? alu?
)


; PART alu-cfg
(struct alu-cfg (absArbiter-cfg absDelay-cfg)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (alu-cfg->string this) port))]
)


(define (init-alu-cfg param-timFct-len param-fanin param-simuCycle
                      param-symType)
  (alu-cfg
    (init-absArbiter-cfg param-timFct-len param-fanin param-symType)
    (cond
      [param-enable-GhostMinion
        (init-absBufferGM-cfg param-timFct-len param-simuCycle param-symType)]
      [else
        (init-absFifo-cfg param-timFct-len param-simuCycle param-symType)]))
)


(define (alu-cfg-evaluate alu-cfg-sym sol)
  (define absArbiter-cfg (alu-cfg-absArbiter-cfg alu-cfg-sym))
  (define absDelay-cfg (alu-cfg-absDelay-cfg alu-cfg-sym))

  (alu-cfg (absArbiter-cfg-evaluate absArbiter-cfg sol)
           (absDelay-cfg-evaluate absDelay-cfg sol))
)


(define (alu-cfg->string alu-cfg)
  "alu-cfg"
)


; PART alu
(struct alu (absArbiter absDelay encoder decoder)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (alu->string this) port))]
)


(define (init-alu alu-cfg f param-operand-len param-reqId-len)
  (define absArbiter-cfg (alu-cfg-absArbiter-cfg alu-cfg))
  (define absDelay-cfg (alu-cfg-absDelay-cfg alu-cfg))

  (define param-entry-len (+ param-reqId-len param-operand-len))
  (define absDelay (init-absDelay absDelay-cfg param-entry-len))
  (define absArbiter (init-absArbiter absArbiter-cfg absDelay param-entry-len))

  (define (encoder reqId operand-a operand-b)
    (concat reqId (f operand-a operand-b)))
  (define (decoder entry)
    (define reqId (extract (sub1 param-entry-len) param-operand-len entry))
    (define result (extract (sub1 param-operand-len) 0 entry))
    (list reqId result)
  )
  
  (alu absArbiter absDelay encoder decoder)
)


(define (alu-dataoutValid alu)
  (define absDelay (alu-absDelay alu))

  (absDelay-dataoutValid absDelay)
)


(define (dataout-alu! alu)
  (define absDelay (alu-absDelay alu))
  (define decoder (alu-decoder alu))

  (decoder (dataout-absDelay! absDelay))
)


(define (datain-alu! alu brID reqId operand-a operand-b fanId timFct)
  (define absArbiter (alu-absArbiter alu))
  (define encoder (alu-encoder alu))

  (datain-absArbiter! absArbiter (encoder reqId operand-a operand-b)
                      brID fanId timFct)
)


(define (alu-datainReady alu)
  (define absArbiter (alu-absArbiter alu))

  (absArbiter-datainReady absArbiter)
)


(define (squash-alu! alu)
  (define absArbiter (alu-absArbiter alu))
  (define absDelay (alu-absDelay alu))

  (squash-absArbiter! absArbiter)
  (1cycleSquash-absDelay! absDelay)
)


(define (squashPartial-alu! alu misPredBr-brID)
  (define absArbiter (alu-absArbiter alu))
  (define absDelay (alu-absDelay alu))

  (squashPartial-absArbiter! absArbiter misPredBr-brID)
  (drainSquashPartial-absDelay! absDelay misPredBr-brID)
)


(define (tick-alu! alu)
  (define absArbiter (alu-absArbiter alu))
  (define absDelay (alu-absDelay alu))

  (when (absArbiter-dataoutValid absArbiter)
    (when param-debug-assert (bug-assert
      (absDelay-datainReady absDelay)
      #:msg "tick-alu!: absDelay-datainReady is False"))
    (match-define (list entry brID timFct) (dataout-absArbiter! absArbiter))
    (datain-absDelay! absDelay entry brID timFct))
  
  (tick-absArbiter! absArbiter)
  (tick-absDelay! absDelay)
)


(define (alu->string alu)
  (define absArbiter (alu-absArbiter alu))
  (define absDelay (alu-absDelay alu))
  
  (~a "\n"
    "  absArbiter: " absArbiter "\n"
    "  absDelay:   " absDelay
  )
)


(define (testMe)


  (define alu-cfg (init-alu-cfg 2 8 10 "func_concrete"))
  (define alu (init-alu alu-cfg bvadd 4 3))
  (printf (~a alu "\n"))

  (printf (~a (alu-dataoutValid alu) "\n"))
  ;(printf (~a (dataout-alu! alu) "\n"))
  (printf (~a (datain-alu! alu (bv 1 param-brID-len) (bv 2 3) (bv 3 4) (bv 5 4)
                           (bv 0 3) (bv 2 2))
              "\n"))
  (printf (~a (alu-datainReady alu) "\n"))
  (printf (~a alu "\n"))

  (tick-alu! alu)
  (printf (~a "-----------------\n"))

  (printf (~a (alu-dataoutValid alu) "\n"))
  (printf (~a (dataout-alu! alu) "\n"))
  (printf (~a (datain-alu! alu (bv 3 param-brID-len) (bv 1 3) (bv 1 4) (bv 1 4)
                           (bv 1 3) (bv 1 2))
              "\n"))
  (printf (~a (alu-datainReady alu) "\n"))
  (printf (~a alu "\n"))
)
;(testMe)

