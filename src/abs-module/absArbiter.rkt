#lang rosette

(require "../lib/lib.rkt" "absDelay.rkt")
(require "absFifo.rkt") ; for debug
(provide
  init-absArbiter-cfg absArbiter-cfg-evaluate
  init-absArbiter absArbiter-dataoutValid dataout-absArbiter!
  datain-absArbiter! absArbiter-datainReady squash-absArbiter!
  squashPartial-absArbiter! tick-absArbiter!
)



; stateless
; module arbiter (
;   input  datain_1;
;   input  datain_id_1;
;   input  datain_valid_1;
;   output datain_ready_1;
;   input  timing_factors_1;

;   input  datain_2;
;   input  datain_id_2;
;   input  datain_valid_2;
;   output datain_ready_2;
;   input  timing_factors_2;

;   ...

;   output dataout;
;   output dataout_id;
;   output dataout_valid;
;   input  dataout_ready;
; )

;; Simulation Constrains:
;  - fanin should be 2^n
;  - we assume arbiter will out valid when there's at least one in valid


(struct absArbiter-cfg (uninterF param-timFct-len param-fanin)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absArbiter-cfg->string this) port))]
)


(define (init-absArbiter-cfg param-timFct-len param-fanin param-symType)
  (define param-fanin-log (inexact->exact (log param-fanin 2)))

  (absArbiter-cfg
    (cond
      [param-enable-arbiterPriority
        (lambda (history-brID history-valid)
          (define fanId (bv 0 param-fanin-log))
          (define brID-mini (bv -1 param-brID-len))
          (for ([i (in-range param-fanin)])
            (define i-bv (integer->bitvector i (bitvector param-fanin-log)))
            (define brID (array-ref history-brID i-bv))
            (define valid (array-ref history-valid i-bv))
            (when (and (bveq valid (bv 1 1)) (bvule brID brID-mini))
              (set! fanId i-bv)
              (set! brID-mini brID)))
          fanId)]

      ; NOTE: this concrete function return the smallest ready fanId
      [(equal? param-symType "func_concrete")
        (lambda (history-timFct-valid)
          (define history-valid (extract (- param-fanin 1) 0
                                         history-timFct-valid))
          (define fanId (bv 0 param-fanin-log))
          (for ([i (in-range (- param-fanin 1) -1 -1)])
            (define i-bv (integer->bitvector i (bitvector param-fanin-log)))
            (define valid (extract i i history-valid))
            (when (bveq valid (bv 1 1)) (set! fanId i-bv)))
          ; (printf (~a "Get: " history-valid "; Return: " fanId "\n"))
          fanId)]

      [(equal? param-symType "func_sym")
        (build-unfuncbv (* param-fanin (add1 param-timFct-len))
                        param-fanin-log)])
    param-timFct-len
    param-fanin)
)


(define (absArbiter-cfg-evaluate absArbiter-cfg-sym sol)
  (define uninterF (absArbiter-cfg-uninterF absArbiter-cfg-sym))
  (define param-timFct-len (absArbiter-cfg-param-timFct-len absArbiter-cfg-sym))
  (define param-fanin (absArbiter-cfg-param-fanin absArbiter-cfg-sym))

  (absArbiter-cfg (evaluate uninterF sol) param-timFct-len param-fanin)
)


(define (absArbiter-cfg->string absArbiter-cfg)
  "uninterF"
)


; PART absArbiter
(struct absArbiter (buffer-data buffer-squashed buffer-brID history-timFct
                    history-brID history-valid uninterF absDelay param-fanin)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absArbiter->string this) port))]
)


(define (init-absArbiter absArbiter-cfg absDelay param-entry-len)
  (define param-timFct-len (absArbiter-cfg-param-timFct-len absArbiter-cfg))
  (define param-fanin (absArbiter-cfg-param-fanin absArbiter-cfg))
  (define param-fanin-log (inexact->exact (ceiling (log param-fanin 2))))
  
  (absArbiter
    (initZeroVec-array param-fanin param-entry-len)
    (initFalseVec-array param-fanin)
    (initZeroVec-array param-fanin param-brID-len)

    (initZeroVec-array param-fanin param-timFct-len)
    (initZeroVec-array param-fanin param-brID-len)
    (initZeroVec-array param-fanin 1)
    (absArbiter-cfg-uninterF absArbiter-cfg)
    absDelay
    param-fanin)
)


(define (datain-absArbiter! absArbiter datain brID fanId timFct)
  (define buffer-data (absArbiter-buffer-data absArbiter))
  (define buffer-squashed (absArbiter-buffer-squashed absArbiter))
  (define buffer-brID (absArbiter-buffer-brID absArbiter))
  (define history-timFct (absArbiter-history-timFct absArbiter))
  (define history-brID (absArbiter-history-brID absArbiter))
  (define history-valid (absArbiter-history-valid absArbiter))
  
  (set-array! buffer-data fanId datain)
  (set-array! buffer-squashed fanId #f)
  (set-array! buffer-brID fanId brID)
  (set-array! history-timFct fanId timFct)
  (when param-enable-arbiterPriority (set-array! history-brID fanId brID))
  (set-array! history-valid fanId (bv 1 1))
)


(define (absArbiter-datainReady absArbiter)
  (define uninterF (absArbiter-uninterF absArbiter))
  (define history-timFct (absArbiter-history-timFct absArbiter))
  (define history-brID (absArbiter-history-brID absArbiter))
  (define history-valid (absArbiter-history-valid absArbiter))
  (define absDelay (absArbiter-absDelay absArbiter))

  (define fanId (if param-enable-arbiterPriority
    (uninterF history-brID history-valid)
    (uninterF (concat (array->bv history-timFct) (array->bv history-valid)))))
  (define fanValid (array-ref history-valid fanId))

  (list (and (absDelay-datainReady absDelay) (bitvector->bool fanValid)) fanId)
)


(define (absArbiter-dataoutValid absArbiter)
  (match-define (list valid fanId) (absArbiter-datainReady absArbiter))
  (define buffer-squashed (absArbiter-buffer-squashed absArbiter))
  
  (and valid (not (array-ref buffer-squashed fanId)))
)


(define (dataout-absArbiter! absArbiter)
  (define buffer-data (absArbiter-buffer-data absArbiter))
  (define buffer-brID (absArbiter-buffer-brID absArbiter))
  (define history-timFct (absArbiter-history-timFct absArbiter))
  (match-define (list valid fanId) (absArbiter-datainReady absArbiter))

  (list (array-ref buffer-data fanId)
        (array-ref buffer-brID fanId)
        (array-ref history-timFct fanId))
)


(define (squash-absArbiter! absArbiter)
  (define buffer-squashed (absArbiter-buffer-squashed absArbiter))

  (resetToTrueVec-array! buffer-squashed)
)


(define (squashPartial-absArbiter! absArbiter misPredBr-brID)
  (define buffer-squashed (absArbiter-buffer-squashed absArbiter))
  (define buffer-brID (absArbiter-buffer-brID absArbiter))
  (define history-valid (absArbiter-history-valid absArbiter))
  (define param-fanin (absArbiter-param-fanin absArbiter))
  (define param-fanin-log (inexact->exact (ceiling (log param-fanin 2))))

  (for ([i (in-range param-fanin)])
    (define i-bv (integer->bitvector i (bitvector param-fanin-log)))

    (define brID (array-ref buffer-brID i-bv))
    (when (bvugt brID misPredBr-brID)
      (set-array! buffer-squashed i-bv #t)))
)


(define (tick-absArbiter! absArbiter)
  (define buffer-data (absArbiter-buffer-data absArbiter))
  (define buffer-squashed (absArbiter-buffer-squashed absArbiter))
  (define buffer-brID (absArbiter-buffer-brID absArbiter))
  (define history-timFct (absArbiter-history-timFct absArbiter))
  (define history-valid (absArbiter-history-valid absArbiter))

  (resetToZeroVec-array! buffer-data)
  (resetToFalseVec-array! buffer-squashed)
  (resetToZeroVec-array! buffer-brID)
  (resetToZeroVec-array! history-timFct)
  (resetToZeroVec-array! history-valid)
)


(define (absArbiter->string absArbiter)
  (~a
    ; "buffer-data: " (absArbiter-buffer-data absArbiter)
    ; "buffer-squashed: " (absArbiter-buffer-squashed absArbiter)
    ; "buffer-brID: " (absArbiter-buffer-brID absArbiter)
    "timFct: " (absArbiter-history-timFct absArbiter)
    "valid: " (absArbiter-history-valid absArbiter)
  )
)


(define (testMe)


  (define absFifo-cfg (init-absFifo-cfg 2 10 "func_concrete"))
  (define absFifo (init-absFifo absFifo-cfg 5))

  (define absArbiter-cfg (init-absArbiter-cfg 2 8 "func_concrete"))
  (define absArbiter (init-absArbiter absArbiter-cfg absFifo 4))
  (printf (~a absArbiter "\n"))

  (printf (~a (absArbiter-dataoutValid absArbiter) "\n"))
  ;(printf (~a (dataout-absArbiter! absArbiter) "\n"))
  (printf (~a (datain-absArbiter! absArbiter (bv 1 5) (bv 0 3) (bv 1 2)) "\n"))
  (printf (~a (absArbiter-datainReady absArbiter) "\n"))
  (printf (~a absArbiter "\n"))

  (tick-absArbiter! absArbiter)
  (printf (~a "-----------------\n"))

  (printf (~a (absArbiter-dataoutValid absArbiter) "\n"))
  (printf (~a (dataout-absArbiter! absArbiter) "\n"))
  (printf (~a (datain-absArbiter! absArbiter (bv 8 5) (bv 1 3) (bv 2 2)) "\n"))
  (printf (~a (absArbiter-datainReady absArbiter) "\n"))
  (printf (~a absArbiter "\n"))
)
;(testMe)

