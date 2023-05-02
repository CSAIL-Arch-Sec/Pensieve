#lang rosette

(require "../lib/lib.rkt")
(provide
  init-absFifo2-cfg absFifo2-cfg-evaluate absFifo2-cfg?
  init-absFifo2 absFifo2-dataoutValid dataout-absFifo2! absFifo2-datainReady
  datain-absFifo2! 1cycleSquash-absFifo2! drainSquash-absFifo2! tick-absFifo2!
  absFifo2?
)


; This just double the input/output bandwidth to 2
; module fifo (
;   input  (datain, dataout_id);
;   output datain_ready;
;   input  timing_factors;
;   input  (datain, dataout_id);
;   output datain_ready;
;   input  timing_factors;

;   output (dataout, dataout_id);
;   output dataout_valid;
;   input  dataout_ready;
;   output (dataout, dataout_id);
;   output dataout_valid;
;   input  dataout_ready;
; )




; PART absFifo2-cfg
(struct absFifo2-cfg (uninterF param-timFct-len param-simuCycle2)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absFifo2-cfg->string this) port))]
)


(define (init-absFifo2-cfg param-timFct-len param-simuCycle param-symType)
  (absFifo2-cfg
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 1 2))]

      [(equal? param-symType "func_sym")
        (build-unfuncbv (* 2 param-simuCycle (add1 param-timFct-len)) 2)])
    param-timFct-len
    (* 2 param-simuCycle))
)


(define (absFifo2-cfg-evaluate absFifo2-cfg-sym sol)
  (define uninterF (absFifo2-cfg-uninterF absFifo2-cfg-sym))
  (define param-timFct-len
    (absFifo2-cfg-param-timFct-len absFifo2-cfg-sym))
  (define param-simuCycle2 (absFifo2-cfg-param-simuCycle2 absFifo2-cfg-sym))

  (absFifo2-cfg (evaluate uninterF sol) param-timFct-len param-simuCycle2)
)


(define (absFifo2-cfg->string absFifo2-cfg)
  "uninterF"
)


; PART absFifo2
(struct absFifo2 (buffer head tail history historyValid uninterF clk
                 param-simuCycle2)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (absFifo2->string this) port))]
)


(define (init-absFifo2 absFifo2-cfg param-entry-len)
  (define param-timFct-len
    (absFifo2-cfg-param-timFct-len absFifo2-cfg))
  (define param-simuCycle2 (absFifo2-cfg-param-simuCycle2 absFifo2-cfg))
  (define param-simuCycle2-log
    (inexact->exact (ceiling (log param-simuCycle2 2))))
  
  (absFifo2
    (initZeroVec-array param-simuCycle2 param-entry-len)
    (bv 0 param-simuCycle2-log)
    (bv 0 param-simuCycle2-log)
    (initZeroVec-array param-simuCycle2 param-timFct-len)
    (initZeroVec-array param-simuCycle2 1)
    (absFifo2-cfg-uninterF absFifo2-cfg)
    (bv 0 (- param-simuCycle2-log 1))
    param-simuCycle2)
)


(define (absFifo2-dataoutValid absFifo2)
  (define head (absFifo2-head absFifo2))
  (define tail (absFifo2-tail absFifo2))
  (define uninterF (absFifo2-uninterF absFifo2))
  (define history (absFifo2-history absFifo2))
  (define historyValid (absFifo2-historyValid absFifo2))

  (define notEmpty (not (bveq head tail)))
  (define valid-bv (uninterF (concat (array->bv history)
                                     (array->bv historyValid))))
  (list (and notEmpty (bitvector->bool (msb valid-bv)))
        (and notEmpty (bitvector->bool (lsb valid-bv))))
)


; NOTE: each call to this function only outputs 1 data each time
(define (dataout-absFifo2! absFifo2)
  (define buffer (absFifo2-buffer absFifo2))
  (define head (absFifo2-head absFifo2))
  
  (set-absFifo2-head! absFifo2 (bvadd1 head))
  (array-ref buffer head)
)


(define (absFifo2-datainReady absFifo2) (list #t #t))


; NOTE: assume both input channels are valid
(define (datain-absFifo2! absFifo2 datain-0 timFct-0 datain-1 timFct-1)
  (define buffer (absFifo2-buffer absFifo2))
  (define tail (absFifo2-tail absFifo2))
  (define history (absFifo2-history absFifo2))
  (define historyValid (absFifo2-historyValid absFifo2))
  (define clk (absFifo2-clk absFifo2))

  (set-array! buffer tail datain-0)
  (set-array! buffer (bvadd1 tail) datain-1)
  (set-absFifo2-tail! absFifo2 (bvadd1 (bvadd1 tail)))
  (set-array! history (concat clk (bv 0 1)) timFct-0)
  (set-array! history (concat clk (bv 1 1)) timFct-1)
  (set-array! historyValid (concat clk (bv 0 1)) (bv 1 1))
  (set-array! historyValid (concat clk (bv 1 1)) (bv 1 1))
)


(define (1cycleSquash-absFifo2! absFifo2)
  (define buffer (absFifo2-buffer absFifo2))
  (define history (absFifo2-history absFifo2))
  (define historyValid (absFifo2-historyValid absFifo2))
  (define param-simuCycle2 (absFifo2-param-simuCycle2 absFifo2))
  (define param-simuCycle2-log
    (inexact->exact (ceiling (log param-simuCycle2 2))))
  
  (resetToZeroVec-array! buffer)
  (set-absFifo2-head! absFifo2 (bv 0 param-simuCycle2-log))
  (set-absFifo2-tail! absFifo2 (bv 0 param-simuCycle2-log))
  (resetToZeroVec-array! history)
  (resetToZeroVec-array! historyValid)
)


(define (drainSquash-absFifo2! absFifo2)
  (define buffer (absFifo2-buffer absFifo2))
  (define param-simuCycle2 (absFifo2-param-simuCycle2 absFifo2))
  (define param-simuCycle2-log
    (inexact->exact (ceiling (log param-simuCycle2 2))))
  
  (resetToZeroVec-array! buffer)
  (set-absFifo2-head! absFifo2 (bv 0 param-simuCycle2-log))
  (set-absFifo2-tail! absFifo2 (bv 0 param-simuCycle2-log))
)


(define (tick-absFifo2! absFifo2)
  (define clk (absFifo2-clk absFifo2))
  
  (set-absFifo2-clk! absFifo2 (bvadd1 clk))
)


(define (absFifo2->string absFifo2)
  (~a (absFifo2-historyValid absFifo2))
)


(define (testMe)

  (define absFifo2-cfg (init-absFifo2-cfg 2 10 "func_concrete"))
  (define absFifo2 (init-absFifo2 absFifo2-cfg 5))
  (printf (~a absFifo2 "\n"))

  (printf (~a (absFifo2-dataoutValid absFifo2) "\n"))
  ;(printf (~a (dataout-absFifo2! absFifo2) "\n"))
  (printf (~a (absFifo2-datainReady absFifo2) "\n"))
  (printf (~a (datain-absFifo2! absFifo2 (bv 31 5) (bv 3 2)) "\n"))
  (printf (~a absFifo2 "\n"))

  (tick-absFifo2! absFifo2)
  (printf (~a "-----------------\n"))

  (printf (~a (absFifo2-dataoutValid absFifo2) "\n"))
  (printf (~a (dataout-absFifo2! absFifo2) "\n"))
  (printf (~a (absFifo2-datainReady absFifo2) "\n"))
  (printf (~a (datain-absFifo2! absFifo2 (bv 1 5) (bv 1 2)) "\n"))
  (printf (~a absFifo2 "\n"))
)
;(testMe)

