#lang rosette

(require "../lib/lib.rkt")
(provide
  init-absBufferGM-cfg absBufferGM-cfg-evaluate absBufferGM-cfg?
  init-absBufferGM absBufferGM-dataoutValid dataout-absBufferGM!
  absBufferGM-datainReady datain-absBufferGM! 1cycleSquash-absBufferGM!
  drainSquash-absBufferGM! drainSquashPartial-absBufferGM! absBufferGM-obsv
  tick-absBufferGM! absBufferGM?
)


; module buffer (
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
;  - This is a delay buffer that can re-order requests


; PART absBufferGM-cfg
(struct absBufferGM-cfg (uninterF param-timFct-len param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absBufferGM-cfg->string this) port))]
)


(define (init-absBufferGM-cfg param-timFct-len param-simuCycle param-symType)
  (absBufferGM-cfg
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 1 1))]

      [(equal? param-symType "func_sym")
        (build-unfuncbv (* param-simuCycle (add1 param-timFct-len)) 1)])
    param-timFct-len
    param-simuCycle)
)


(define (absBufferGM-cfg-evaluate absBufferGM-cfg-sym sol)
  (define uninterF (absBufferGM-cfg-uninterF absBufferGM-cfg-sym))
  (define param-timFct-len
    (absBufferGM-cfg-param-timFct-len absBufferGM-cfg-sym))
  (define param-simuCycle (absBufferGM-cfg-param-simuCycle absBufferGM-cfg-sym))

  (absBufferGM-cfg (evaluate uninterF sol) param-timFct-len param-simuCycle)
)


(define (absBufferGM-cfg->string absBufferGM-cfg)
  "uninterF"
)


; PART absBufferGM
(struct absBufferGM (buffer-data buffer-brID buffer-valid history-timFct
                     history-valid uninterF clk param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absBufferGM->string this) port))]
)


(define (init-absBufferGM absBufferGM-cfg param-entry-len)
  (define uninterF (absBufferGM-cfg-uninterF absBufferGM-cfg))
  (define param-timFct-len
    (absBufferGM-cfg-param-timFct-len absBufferGM-cfg))
  (define param-simuCycle (absBufferGM-cfg-param-simuCycle absBufferGM-cfg))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (absBufferGM
    (initZeroVec-array param-simuCycle param-entry-len)
    (initZeroVec-array param-simuCycle param-brID-len)
    (initZeroVec-array param-simuCycle 1)

    (initZeroVec-array param-simuCycle param-timFct-len)
    (initZeroVec-array param-simuCycle 1)
    uninterF

    (bv 0 param-simuCycle-log)
    param-simuCycle)
)


(define (absBufferGM-nextOut-isEmpty-index-brID absBufferGM)
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  (define param-simuCycle (absBufferGM-param-simuCycle absBufferGM))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))

  (define isEmpty #t)
  (define index (bv 0 param-simuCycle-log))
  (define brID-mini (bv -1 param-brID-len))
  (for ([i (in-range param-simuCycle)])
    (define i-bv (integer->bitvector i (bitvector param-simuCycle-log)))
    (define brID (array-ref buffer-brID i-bv))
    (define valid (array-ref buffer-valid i-bv))
    (when (and (bveq valid (bv 1 1)) (bvule brID brID-mini))
      (set! isEmpty #f)
      (set! index i-bv)
      (set! brID-mini brID)))

  (list isEmpty index brID-mini)
)


(define (absBufferGM-dataoutValid absBufferGM)
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  (define history-timFct (absBufferGM-history-timFct absBufferGM))
  (define history-valid (absBufferGM-history-valid absBufferGM))
  (define uninterF (absBufferGM-uninterF absBufferGM))

  (match-define (list isEmpty index brID-mini)
                (absBufferGM-nextOut-isEmpty-index-brID absBufferGM))

  ; NOTE: we assume buffer and history are indexed in a same way here
  (define (maskThisEntry i-bv)
    (define brID (array-ref buffer-brID i-bv))
    (bvugt brID brID-mini))
  (define history-timFct-masked (array->masked history-timFct maskThisEntry))
  (define history-valid-masked (array->masked history-valid maskThisEntry))

  (and (not isEmpty)
       (bitvector->bool (uninterF (concat (array->bv history-timFct-masked)
                                          (array->bv history-valid-masked)))))
)


(define (dataout-absBufferGM! absBufferGM)
  (define buffer-data (absBufferGM-buffer-data absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))

  (match-define (list isEmpty index brID-mini)
                (absBufferGM-nextOut-isEmpty-index-brID absBufferGM))

  (resetToZero-array! buffer-valid index)
  (array-ref buffer-data index)
)


(define (absBufferGM-datainReady absBufferGM) #t)


(define (datain-absBufferGM! absBufferGM datain brID timFct)
  (define buffer-data (absBufferGM-buffer-data absBufferGM))
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  (define history-timFct (absBufferGM-history-timFct absBufferGM))
  (define history-valid (absBufferGM-history-valid absBufferGM))
  (define clk (absBufferGM-clk absBufferGM))

  (set-array! buffer-data clk datain)
  (set-array! buffer-brID clk brID)
  (set-array! buffer-valid clk (bv 1 1))

  (set-array! history-timFct clk timFct)
  (set-array! history-valid clk (bv 1 1))
)


; NOTE: 1cycleSquash cannot be partial
(define (1cycleSquash-absBufferGM! absBufferGM)
  (define buffer-data (absBufferGM-buffer-data absBufferGM))
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  (define history-timFct (absBufferGM-history-timFct absBufferGM))
  (define history-valid (absBufferGM-history-valid absBufferGM))
  
  (resetToZeroVec-array! buffer-data)
  (resetToZeroVec-array! buffer-brID)
  (resetToZeroVec-array! buffer-valid)
  (resetToZeroVec-array! history-timFct)
  (resetToZeroVec-array! history-valid)
)


(define (drainSquash-absBufferGM! absBufferGM)
  (define buffer-data (absBufferGM-buffer-data absBufferGM))
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  
  (resetToZeroVec-array! buffer-data)
  (resetToZeroVec-array! buffer-brID)
  (resetToZeroVec-array! buffer-valid)
)


(define (drainSquashPartial-absBufferGM! absBufferGM misPredBr-brID)
  (define buffer-data (absBufferGM-buffer-data absBufferGM))
  (define buffer-brID (absBufferGM-buffer-brID absBufferGM))
  (define buffer-valid (absBufferGM-buffer-valid absBufferGM))
  (define param-simuCycle (absBufferGM-param-simuCycle absBufferGM))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (for ([i (in-range param-simuCycle)])
    (define i-bv (integer->bitvector i (bitvector param-simuCycle-log)))

    (define brID (array-ref buffer-brID i-bv))
    (when (bvugt brID misPredBr-brID)
      (resetToZero-array! buffer-data i-bv)
      (resetToZero-array! buffer-brID i-bv)
      (resetToZero-array! buffer-valid i-bv)))
)


(define (absBufferGM-obsv absBufferGM)
  (define history-timFct (absBufferGM-history-timFct absBufferGM))
  (define history-valid (absBufferGM-history-valid absBufferGM))

  (concat (array->bv history-timFct) (array->bv history-valid))
)


(define (tick-absBufferGM! absBufferGM)
  (define clk (absBufferGM-clk absBufferGM))

  (set-absBufferGM-clk! absBufferGM (bvadd1 clk))
)


(define (absBufferGM->string absBufferGM)
  (~a "history-valid: " (absBufferGM-history-valid absBufferGM)
      "history-timFct: " (absBufferGM-history-timFct absBufferGM)
      ; "buffer-data: " (absBufferGM-buffer-data absBufferGM)
      "buffer-valid: " (absBufferGM-buffer-valid absBufferGM)
      "buffer-brID: " (absBufferGM-buffer-brID absBufferGM)
      ; "dataoutValid: " (absBufferGM-dataoutValid absBufferGM)
  )
)


(define (testMe)

  (define absBufferGM-cfg (init-absBufferGM-cfg 2 10 "func_concrete"))
  (define absBufferGM (init-absBufferGM absBufferGM-cfg 5))
  (printf (~a absBufferGM "\n"))

  (printf (~a (absBufferGM-dataoutValid absBufferGM) "\n"))
  ;(printf (~a (dataout-absBufferGM! absBufferGM) "\n"))
  (printf (~a (absBufferGM-datainReady absBufferGM) "\n"))
  (printf (~a (datain-absBufferGM! absBufferGM (bv 31 5) (bv 3 2)) "\n"))
  (printf (~a absBufferGM "\n"))

  (tick-absBufferGM! absBufferGM)
  (printf (~a "-----------------\n"))

  (printf (~a (absBufferGM-dataoutValid absBufferGM) "\n"))
  (printf (~a (dataout-absBufferGM! absBufferGM) "\n"))
  (printf (~a (absBufferGM-datainReady absBufferGM) "\n"))
  (printf (~a (datain-absBufferGM! absBufferGM (bv 1 5) (bv 1 2)) "\n"))
  (printf (~a absBufferGM "\n"))
)
;(testMe)

