#lang rosette

(require "../lib/lib.rkt" "../inst.rkt")
(provide
  init-memi-cfg memi-cfg-evaluate
  init-memi memi-ref logref-memi! tick-memi! memi-history->string memi-history
  memi?
)


; module memi (
;   input  addr;
;   output dataout;
; )


; PART memi-cfg
(struct memi-cfg (array param-size)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (memi-cfg->string this) port))]
)


(define (init-memi-cfg param-size param-rs1-len param-rs2-len param-rd-len
                       param-symType)
  (define param-size-log (inexact->exact (log param-size 2)))
  (define param-len (+ inst-size-log param-rs1-len param-rs2-len param-rd-len))

  (define array
    (cond
      [(equal? param-symType "vec_concrete")
        (define (initFromInt-inst op rs1 rs2 rd) (init-inst
          op
          (integer->bitvector rs1 (bitvector param-rs1-len))
          (integer->bitvector rs2 (bitvector param-rs2-len))
          (integer->bitvector rd  (bitvector param-rd-len))))
        (init-array 
          (vector-immutable
            (initFromInt-inst inst-op-Ld  0 0 0)
            (initFromInt-inst inst-op-Add 1 1 1)
            (initFromInt-inst inst-op-Br  2 1 0)
            (initFromInt-inst inst-op-Ld  0 0 0)
            (initFromInt-inst inst-op-Ld  2 0 2)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0)
            (initFromInt-inst inst-op-Li  0 0 0))
          param-size
          param-len)]
      
      [(equal? param-symType "vec_sym")
        (init-array 
          (vector->immutable-vector (build-vector
            param-size
            (lambda (ignore)
              (define inst (init-syminst))
              (assume-inst inst)
              inst)))
          param-size
          param-len)]

      [(equal? param-symType "func_sym")
        (define inst-size-log (inexact->exact (ceiling (log inst-size 2))))
        (define op  (build-unfuncbv param-size-log inst-size-log))
        (define rs1 (build-unfuncbv param-size-log param-rs1-len))
        (define rs2 (build-unfuncbv param-size-log param-rs2-len))
        (define rd  (build-unfuncbv param-size-log param-rd-len))
        (define (array-content pos)
          (init-inst (op pos) (rs1 pos) (rs2 pos) (rd pos)))
    
        (for ([pos (in-range param-size)])
          (assume-inst (array-content (bv pos param-size-log))))
        
        (define memi-array (init-array array-content param-size param-len))
        memi-array]

      [else (bug-assert #f #:msg "memi: param-symType unknown")]))

  (memi-cfg array param-size)
)


(define (memi-cfg-evaluate memi-cfg-sym sol)
  (define array (memi-cfg-array memi-cfg-sym))
  (define param-size (memi-cfg-param-size memi-cfg-sym))

  (memi-cfg (array-evaluate array inst-evaluate sol) param-size)
)


(define (memi-cfg->string memi-cfg)
  (define array (memi-cfg-array memi-cfg))
  
  (array->custom-string
    array
    (lambda (i e) (~a (bitvector->natural i) "-th INST " e))
    "\n")
)


; PART memi
; NOTE: the i-th is to help log two memi read per cycle for OoO
(struct memi (array history clk i-th param-size param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (memi->string this) port))]
)


(define (init-memi memi-cfg param-simuCycle)
  (define array (memi-cfg-array memi-cfg))
  (define param-size (memi-cfg-param-size memi-cfg))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))

  (memi
    array
    (initZeroVec-array (* 2 param-simuCycle) (add1 param-size-log))
    (bv 0 param-simuCycle-log)
    (bv 0 1)
    param-size
    param-simuCycle)
)


(define (memi-ref memi pos)
  (define array (memi-array memi))

  (array-ref array pos)
)


(define (logref-memi! memi pos)
  (define history (memi-history memi))
  (define clk (memi-clk memi))
  (define i-th (memi-i-th memi))
  (set-memi-i-th! memi (bv 1 1))

  (set-array! history (concat clk i-th) (concat (bv 1 1) pos))
  (memi-ref memi pos)
)


(define (tick-memi! memi)
  (define clk (memi-clk memi))

  (set-memi-clk! memi (bvadd1 clk))
  (set-memi-i-th! memi (bv 0 1))
)


(define (memi-history->string memi)
  (define history (memi-history memi))
  (define param-size (memi-param-size memi))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))

  (define (indexEntry->string index entry)
    (define valid (extract param-size-log param-size-log entry))
    (define addr (extract (sub1 param-size-log) 0 entry))
    (if (bveq valid (bv 1 1))
        ;(~a (bitvector->natural index) ": " (bitvector->natural addr) ", ")
        (~a (bitvector->natural addr) " ")
        "")
  )

  (array->custom-string history indexEntry->string "")
)


(define (memi->string memi)
  (~a "history: " (memi-history->string memi))
)


(define (testMe)

  (define memi-cfg
    (init-memi-cfg param-memi-size param-reg-len param-rf-size-log
                   param-rf-size-log "vec_concrete"))
  (define memi (init-memi memi-cfg 10))
  (printf (~a memi "\n"))

  (printf (~a (logref-memi! memi (bv 1 param-memi-size-log)) "\n"))
  (printf (~a memi "\n"))

  (tick-memi! memi)

  (printf (~a (logref-memi! memi (bv 4 param-memi-size-log)) "\n"))
  (printf (~a memi "\n"))
)
;(testMe)

