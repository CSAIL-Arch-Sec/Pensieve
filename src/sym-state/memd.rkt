#lang rosette

(require "../lib/lib.rkt")
(provide
  init-memd-cfg-pair memd-cfg-evaluate
  init-memd memd-ref logref-memd! set-memd! logset-memd! tick-memd!
  memd-history->string memd-history
  memd?
)


(define param-sec-len 1)


; module memd (
;   input  datain;
;   input  addr;
;   input  valid;

;   output dataout;
; )


; PART memd-cfg
(struct memd-cfg (array param-size)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (memd-cfg->string this) port))]
)


(define (init-memd-cfg-pair param-size param-len param-symType)
  (define param-size-log (inexact->exact (log param-size 2)))

  (match-define (list array-0 array-1)
    (cond
      [(equal? param-symType "vec_concrete")
        (list
          (init-array (vector (bv 0 param-len)
                              (bv 0 param-len)
                              (bv 0 param-len)
                              (bv 0 param-len))
                      param-size
                      param-len)
          (init-array (vector (bv 1 param-len)
                              (bv 0 param-len)
                              (bv 0 param-len)
                              (bv 0 param-len))
                      param-size
                      param-len))]

      [(equal? param-symType "vec_sym")
        (define array-pubbit (build-symbv (- param-len param-sec-len)))
        (define array-secbit (build-symbv param-sec-len))
        (define array-secentry-0 (concat array-pubbit array-secbit))
        (define array-secentry-1 (concat array-pubbit (bvnot array-secbit)))
        (define array-pubentries (build-vector
          (sub1 param-size)
          (lambda (ignore) (build-symbv param-len))))

        (list
          (init-array (vector-append (vector array-secentry-0)
                                     array-pubentries)
                      param-size
                      param-len)
          (init-array (vector-append (vector array-secentry-1)
                                     array-pubentries)
                      param-size
                      param-len))]
      
      [(equal? param-symType "func_sym")
        (define array-secbit (build-symbv param-sec-len))
        (define array-pub (build-unfuncbv param-size-log param-len))
        (list
          (init-array
            (lambda (x) (if (bvzero? x)
              (concat (extract (sub1 param-len) param-sec-len (array-pub x))
                      array-secbit)
              (array-pub x)))
            param-size
            param-len)
          (init-array
            (lambda (x) (if (bvzero? x)
              (concat (extract (sub1 param-len) param-sec-len (array-pub x))
                      (bvnot array-secbit))
              (array-pub x)))
            param-size
            param-len))]

      [else (bug-assert #f #:msg "memd: param-symType unknown")]))

  (list (memd-cfg array-0 param-size)
        (memd-cfg array-1 param-size))
)


(define (memd-cfg-evaluate memd-cfg-sym sol)
  (define array (memd-cfg-array memd-cfg-sym))
  (define param-size (memd-cfg-param-size memd-cfg-sym))

  (memd-cfg (array-evaluate array evaluate sol) param-size)
)


(define (memd-cfg->string memd-cfg)
  (define array (memd-cfg-array memd-cfg))
  
  (array->custom-string array (lambda (i e) (bitvector->natural e)) " ")
)


; PART memd
(struct memd (array history clk param-size param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (memd->string this) port))]
)


(define (init-memd memd-cfg param-simuCycle)
  (define array (memd-cfg-array memd-cfg))
  (define param-size (memd-cfg-param-size memd-cfg))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))

  (memd
    (initFromCopy-array array)
    (initZeroVec-array param-simuCycle (add1 param-size-log))
    (bv 0 param-simuCycle-log)
    param-size
    param-simuCycle)
)


(define (memd-ref memd pos)
  (define array (memd-array memd))

  (array-ref array pos)
)


(define (logref-memd! memd pos)
  (define history (memd-history memd))
  (define clk (memd-clk memd))

  (set-array! history clk (concat (bv 1 1) pos))
  (memd-ref memd pos)
)


(define (set-memd! memd pos v)
  (define array (memd-array memd))

  (set-array! array pos v)
)


(define (logset-memd! memd pos v)
  (define history (memd-history memd))
  (define clk (memd-clk memd))

  (set-array! history clk (concat (bv 1 1) pos))
  (set-memd! memd pos v)
)


(define (tick-memd! memd)
  (define clk (memd-clk memd))

  (set-memd-clk! memd (bvadd1 clk))
)


(define (memd-history->string memd)
  (define history (memd-history memd))
  (define param-size (memd-param-size memd))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))

  (define (indexEntry->string index entry)
    (define valid (extract param-size-log param-size-log entry))
    (define addr (extract (sub1 param-size-log) 0 entry))
    (if (bveq valid (bv 1 1))
        (~a (bitvector->natural index) ": " (bitvector->natural addr) ", ")
        "")
  )

  (array->custom-string history indexEntry->string "")
)


(define (memd-array->string memd)
  (define array (memd-array memd))
  
  (array->custom-string array (lambda (i e) (bitvector->natural e)) " ")
)


(define (memd->string memd)
  (~a "history: " (memd-history->string memd) "  "
      "array: "   (memd-array->string memd))
)


(define (testMe)

  (match-define (list memd-cfg-0 memd-cfg-1)
                (init-memd-cfg-pair param-memd-size param-reg-len
                                    "vec_concrete"))
  (define memd (init-memd memd-cfg-0 10))
  (printf (~a memd "\n"))

  (printf (~a (logref-memd! memd (bv 1 2)) "\n"))
  (printf (~a (logset-memd! memd (bv 1 2) (bv 7 param-reg-len)) "\n"))
  (printf (~a memd "\n"))

  (tick-memd! memd)

  (printf (~a (logref-memd! memd (bv 1 2)) "\n"))
  (printf (~a (logset-memd! memd (bv 0 2) (bv 3 param-reg-len)) "\n"))
  (printf (~a memd "\n"))
)
;(testMe)

