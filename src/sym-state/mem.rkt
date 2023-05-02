#lang rosette

(require "memi.rkt" "memd.rkt" "../lib/lib.rkt")
(provide mem-ref logref-mem! set-mem! logset-mem! tick-mem!)


(define (mem-ref . arg)
  (cond
    [(memi? (car arg)) (apply memi-ref arg)]
    [(memd? (car arg)) (apply memd-ref arg)]
    [else (bug-assert #f #:msg "mem unknown")])
)


(define (logref-mem! . arg)
  (cond
    [(memi? (car arg)) (apply logref-memi! arg)]
    [(memd? (car arg)) (apply logref-memd! arg)]
    [else (bug-assert #f #:msg "mem unknown")])
)


(define (set-mem! . arg)
  (cond
    [(memd? (car arg)) (apply set-memd! arg)]
    [else (bug-assert #f #:msg "mem unknown")])
)


(define (logset-mem! . arg)
  (cond
    [(memd? (car arg)) (apply logset-memd! arg)]
    [else (bug-assert #f #:msg "mem unknown")])
)


(define (tick-mem! . arg)
  (cond
    [(memi? (car arg)) (apply tick-memi! arg)]
    [(memd? (car arg)) (apply tick-memd! arg)]
    [else (bug-assert #f #:msg "mem unknown")])
)

