#lang rosette

(require "../lib/lib.rkt")
(provide
  init-absFifo-cfg absFifo-cfg-evaluate absFifo-cfg?
  init-absFifo absFifo-dataoutValid dataout-absFifo! absFifo-datainReady
  datain-absFifo! 1cycleSquash-absFifo! drainSquash-absFifo!
  drainSquashPartial-absFifo! absFifo-obsv tick-absFifo! absFifo?
)


; module fifo (
;   input  (datain, datain_id);
;   output datain_ready;
;   input  datain_valid;
;   input  timing_factors;

;   output (dataout, dataout_id);
;   output dataout_valid;
;   input  dataout_ready;
; )

;; Simulation Constrains:
;  - Call valid/ready function to check availablity first, then call dataout/in.
;  - Call dataout first, then datain,
;    to avoid respond the same request at the cycle.
;  - Only push into param-simuCycle number datain,
;    and should not ask dataout after the last datain.
;    Because we simply use head==tail to detect full
;  - This is a fifo, so id is just outputed in-order


; PART absFifo-cfg
(struct absFifo-cfg (uninterF param-timFct-len param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absFifo-cfg->string this) port))]
)


(define (init-absFifo-cfg param-timFct-len param-simuCycle param-symType)
  (absFifo-cfg
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 1 1))]

      [(equal? param-symType "func_sym")
        (build-unfuncbv (* param-simuCycle (add1 param-timFct-len)) 1)])
    param-timFct-len
    param-simuCycle)
)


(define (absFifo-cfg-evaluate absFifo-cfg-sym sol)
  (define uninterF (absFifo-cfg-uninterF absFifo-cfg-sym))
  (define param-timFct-len (absFifo-cfg-param-timFct-len absFifo-cfg-sym))
  (define param-simuCycle (absFifo-cfg-param-simuCycle absFifo-cfg-sym))

  (absFifo-cfg (evaluate uninterF sol) param-timFct-len param-simuCycle)
)


(define (absFifo-cfg->string absFifo-cfg)
  "uninterF"
)


; PART absFifo
(struct absFifo (buffer-data buffer-squashed buffer-brID head tail
                 history-timFct history-valid uninterF clk param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absFifo->string this) port))]
)


(define (init-absFifo absFifo-cfg param-entry-len)
  (define param-timFct-len
    (absFifo-cfg-param-timFct-len absFifo-cfg))
  (define param-simuCycle (absFifo-cfg-param-simuCycle absFifo-cfg))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (absFifo
    (initZeroVec-array  param-simuCycle param-entry-len)
    (initFalseVec-array param-simuCycle)
    (initZeroVec-array  param-simuCycle param-brID-len)
    (bv 0 param-simuCycle-log)
    (bv 0 param-simuCycle-log)
    (initZeroVec-array param-simuCycle param-timFct-len)
    (initZeroVec-array param-simuCycle 1)
    (absFifo-cfg-uninterF absFifo-cfg)
    (bv 0 param-simuCycle-log)
    param-simuCycle)
)


(define (absFifo-empty absFifo)
  (define head (absFifo-head absFifo))
  (define tail (absFifo-tail absFifo))

  (bveq head tail)
)


(define (absFifo-dataoutValid absFifo)
  (define buffer-squashed (absFifo-buffer-squashed absFifo))
  (define head (absFifo-head absFifo))
  (define uninterF (absFifo-uninterF absFifo))
  (define history-timFct (absFifo-history-timFct absFifo))
  (define history-valid (absFifo-history-valid absFifo))

  (and (not (absFifo-empty absFifo))
       (not (array-ref buffer-squashed head))
       (bitvector->bool (uninterF (concat (array->bv history-timFct)
                                          (array->bv history-valid)))))
)


(define (dataout-absFifo! absFifo)
  (define buffer-data (absFifo-buffer-data absFifo))
  (define head (absFifo-head absFifo))

  (when param-debug-assert (bug-assert
    (absFifo-dataoutValid absFifo)
    #:msg "dataout-absFifo!: test dataout_valid before dataout"))
  (set-absFifo-head! absFifo (bvadd1 head))
  (array-ref buffer-data head)
)


(define (absFifo-datainReady absFifo) #t)


(define (datain-absFifo! absFifo datain brID timFct)
  (define buffer-data (absFifo-buffer-data absFifo))
  (define buffer-squashed (absFifo-buffer-squashed absFifo))
  (define buffer-brID (absFifo-buffer-brID absFifo))
  (define tail (absFifo-tail absFifo))
  (define history-timFct (absFifo-history-timFct absFifo))
  (define history-valid (absFifo-history-valid absFifo))
  (define clk (absFifo-clk absFifo))

  (set-array! buffer-data tail datain)
  (set-array! buffer-squashed tail #f)
  (set-array! buffer-brID tail brID)
  (set-absFifo-tail! absFifo (bvadd1 tail))

  (set-array! history-timFct clk timFct)
  (set-array! history-valid clk (bv 1 1))
)


; NOTE: 1cycleSquash cannot be partial
(define (1cycleSquash-absFifo! absFifo)
  (define buffer-data (absFifo-buffer-data absFifo))
  (define history-timFct (absFifo-history-timFct absFifo))
  (define history-valid (absFifo-history-valid absFifo))
  (define param-simuCycle (absFifo-param-simuCycle absFifo))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (resetToZeroVec-array! buffer-data)
  (set-absFifo-head! absFifo (bv 0 param-simuCycle-log))
  (set-absFifo-tail! absFifo (bv 0 param-simuCycle-log))
  (resetToZeroVec-array! history-timFct)
  (resetToZeroVec-array! history-valid)
)


(define (drainSquash-absFifo! absFifo)
  (define buffer-data (absFifo-buffer-data absFifo))
  (define param-simuCycle (absFifo-param-simuCycle absFifo))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (resetToZeroVec-array! buffer-data)
  (set-absFifo-head! absFifo (bv 0 param-simuCycle-log))
  (set-absFifo-tail! absFifo (bv 0 param-simuCycle-log))
)


; TODO: can we have a version to change the head and tail of the buffer
;       instead of using squahsed bit?
(define (drainSquashPartial-absFifo! absFifo misPredBr-brID)
  (define buffer-data (absFifo-buffer-data absFifo))
  (define buffer-squashed (absFifo-buffer-squashed absFifo))
  (define buffer-brID (absFifo-buffer-brID absFifo))
  (define param-simuCycle (absFifo-param-simuCycle absFifo))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (for ([i (in-range param-simuCycle)])
    (define i-bv (integer->bitvector i (bitvector param-simuCycle-log)))

    (define brID (array-ref buffer-brID i-bv))
    (when (bvugt brID misPredBr-brID)
      (set-array! buffer-squashed i-bv #t)))
)


(define (absFifo-obsv absFifo)
  (define history-timFct (absFifo-history-timFct absFifo))
  (define history-valid (absFifo-history-valid absFifo))

  (concat (array->bv history-timFct) (array->bv history-valid))
)


(define (tick-absFifo! absFifo)
  (define clk (absFifo-clk absFifo))
  (define buffer-squashed (absFifo-buffer-squashed absFifo))
  (define head (absFifo-head absFifo))
  
  (when (and (not (absFifo-empty absFifo)) (array-ref buffer-squashed head))
    (set-absFifo-head! absFifo (bvadd1 head)))
  (set-absFifo-clk! absFifo (bvadd1 clk))

)


(define (absFifo->string absFifo)
  (~a "Valid: " (absFifo-history-valid absFifo)
      "timFct: " (absFifo-history-timFct absFifo)
      ; "buffer-data: " (absFifo-buffer-data absFifo)
      "buffer-brID: " (absFifo-buffer-brID absFifo)
      ; "buffer-squashed: " (absFifo-buffer-squashed absFifo)
      ; "dataoutValid: " (absFifo-dataoutValid absFifo)
  )
)


(define (testMe)

  (define absFifo-cfg (init-absFifo-cfg 2 10 "func_concrete"))
  (define absFifo (init-absFifo absFifo-cfg 5))
  (printf (~a absFifo "\n"))

  (printf (~a (absFifo-dataoutValid absFifo) "\n"))
  ;(printf (~a (dataout-absFifo! absFifo) "\n"))
  (printf (~a (absFifo-datainReady absFifo) "\n"))
  (printf (~a (datain-absFifo! absFifo (bv 31 5) (bv 3 2)) "\n"))
  (printf (~a absFifo "\n"))

  (tick-absFifo! absFifo)
  (printf (~a "-----------------\n"))

  (printf (~a (absFifo-dataoutValid absFifo) "\n"))
  (printf (~a (dataout-absFifo! absFifo) "\n"))
  (printf (~a (absFifo-datainReady absFifo) "\n"))
  (printf (~a (datain-absFifo! absFifo (bv 1 5) (bv 1 2)) "\n"))
  (printf (~a absFifo "\n"))
)
;(testMe)

