#lang rosette

(require "../lib/lib.rkt")
(provide init-rf-cfg rf-cfg-evaluate init-rf resetsym-rf! rf-ref set-rf!)


; PART rf-cfg
(struct rf-cfg (array param-size param-len)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (rf-cfg->string this) port))]
)


(define (init-rf-cfg param-size param-len param-symType)
  (define param-size-log (inexact->exact (log param-size 2)))

  (define array
    (cond
      [(equal? param-symType "vec_concrete")
        (init-array (vector (bv 3 param-len)
                            (bv 1 param-len)
                            (bv 1 param-len)
                            (bv 0 param-len))
                    param-size
                    param-len)]
      [(equal? param-symType "vec_sym")
        (init-array (build-vector param-size
                                  (lambda (ignore) (build-symbv param-len)))
                    param-size
                    param-len)]
      [(equal? param-symType "func_concrete")
        (init-array (lambda (pos) (if (bvzero? pos)
                                      (bv 1 param-len)
                                      (bv 0 param-len)))
                    param-size
                    param-len)]
      [(equal? param-symType "func_sym")
        (init-array (build-unfuncbv param-size-log param-len)
                    param-size param-len)]
      [else (bug-assert #f #:msg "rf: param-symType unknown")]))

  (rf-cfg array param-size param-len)
)


(define (rf-cfg-evaluate rf-cfg-sym sol)
  (define array (rf-cfg-array rf-cfg-sym))
  (define param-size (rf-cfg-param-size rf-cfg-sym))
  (define param-len (rf-cfg-param-len rf-cfg-sym))

  (rf-cfg (array-evaluate array evaluate sol) param-size param-len)
)


(define (rf-cfg->string rf-cfg)
  (define array (rf-cfg-array rf-cfg))
  
  (array->custom-string array (lambda (i e) (bitvector->natural e)) " ")
)


; PART rf
(struct rf (array param-size param-len)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (rf->string this) port))]
)


(define (init-rf rf-cfg)
  (define array (rf-cfg-array rf-cfg))
  (define param-size (rf-cfg-param-size rf-cfg))
  (define param-len (rf-cfg-param-len rf-cfg))
  
  (rf
    (initFromCopy-array array)
    param-size
    param-len)
)


(define (resetsym-rf! rf)
  (define array (rf-array rf))
  (define param-size (rf-param-size rf))
  (define param-len (rf-param-len rf))

  (define rf-cfg-new (init-rf-cfg param-size param-len "vec_sym"))
  (define array-new (rf-cfg-array rf-cfg-new))
  (assume (array-equal array array-new))
  (set-rf-array! rf array-new)
)


(define (rf-ref rf pos)
  (define array (rf-array rf))

  (array-ref array pos)
)


(define (set-rf! rf pos v)
  (define array (rf-array rf))

  (set-array! array pos v)
)


(define (rf->string rf)
  (define array (rf-array rf))
  
  (array->custom-string array (lambda (i e) (bitvector->natural e)) " ")
)


(define (testMe)

  (define rf-cfg (init-rf-cfg param-rf-size param-reg-len "vec_concrete"))
  (define rf (init-rf rf-cfg))
  (printf (~a rf "\n"))

  (printf (~a (rf-ref rf (bv 1 param-rf-size-log)) "\n"))
  (printf (~a (set-rf! rf (bv 1 param-rf-size-log) (bv 7 param-reg-len)) "\n"))
  (printf (~a (set-rf! rf (bv 2 param-rf-size-log) (bv 2 param-reg-len)) "\n"))
  (printf (~a rf "\n"))
)
;(testMe)

