#lang rosette

(require "absFifo.rkt" "absFifo2.rkt" "absBufferGM.rkt" "../lib/lib.rkt")
(provide
   absDelay-cfg-evaluate init-absDelay absDelay-dataoutValid dataout-absDelay!
   absDelay-datainReady datain-absDelay! 1cycleSquash-absDelay!
   drainSquash-absDelay! drainSquashPartial-absDelay! absDelay-obsv
   tick-absDelay!
)


(define (absDelay-cfg-evaluate . arg)
  (cond
    [(absFifo-cfg? (car arg)) (apply absFifo-cfg-evaluate arg)]
    [(absFifo2-cfg? (car arg)) (apply absFifo2-cfg-evaluate arg)]
    [(absBufferGM-cfg? (car arg)) (apply absBufferGM-cfg-evaluate arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (init-absDelay . arg)
  (cond
    [(absFifo-cfg? (car arg)) (apply init-absFifo arg)]
    [(absFifo2-cfg? (car arg)) (apply init-absFifo2 arg)]
    [(absBufferGM-cfg? (car arg)) (apply init-absBufferGM arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (absDelay-dataoutValid . arg)
  (cond
    [(absFifo? (car arg)) (apply absFifo-dataoutValid arg)]
    [(absFifo2? (car arg)) (apply absFifo2-dataoutValid arg)]
    [(absBufferGM? (car arg)) (apply absBufferGM-dataoutValid arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (dataout-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply dataout-absFifo! arg)]
    [(absFifo2? (car arg)) (apply dataout-absFifo2! arg)]
    [(absBufferGM? (car arg)) (apply dataout-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (absDelay-datainReady . arg)
  (cond
    [(absFifo? (car arg)) (apply absFifo-datainReady arg)]
    [(absFifo2? (car arg)) (apply absFifo2-datainReady arg)]
    [(absBufferGM? (car arg)) (apply absBufferGM-datainReady arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (datain-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply datain-absFifo! arg)]
    [(absFifo2? (car arg)) (apply datain-absFifo2! arg)]
    [(absBufferGM? (car arg)) (apply datain-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (1cycleSquash-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply 1cycleSquash-absFifo! arg)]
    [(absFifo2? (car arg)) (apply 1cycleSquash-absFifo2! arg)]
    [(absBufferGM? (car arg)) (apply 1cycleSquash-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (drainSquash-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply drainSquash-absFifo! arg)]
    [(absFifo2? (car arg)) (apply drainSquash-absFifo2! arg)]
    [(absBufferGM? (car arg)) (apply drainSquash-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (drainSquashPartial-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply drainSquashPartial-absFifo! arg)]
    [(absBufferGM? (car arg)) (apply drainSquashPartial-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (absDelay-obsv . arg)
  (cond
    [(absFifo? (car arg)) (apply absFifo-obsv arg)]
    [(absBufferGM? (car arg)) (apply absBufferGM-obsv arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)


(define (tick-absDelay! . arg)
  (cond
    [(absFifo? (car arg)) (apply tick-absFifo! arg)]
    [(absFifo2? (car arg)) (apply tick-absFifo2! arg)]
    [(absBufferGM? (car arg)) (apply tick-absBufferGM! arg)]
    [else (bug-assert #f #:msg "absDelay unknown")])
)

