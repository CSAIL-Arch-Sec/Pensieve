#lang rosette

(require "lib/lib.rkt")

(provide
  inst-size inst-size-log inst-len
  inst-op-Li inst-op-Add inst-op-Mul
  inst-op-Ld inst-op-St  inst-op-Br
  inst-useSt
  (struct-out inst)
  init-inst init-syminst bv->inst assume-inst inst-evaluate
)


; PART Parameters
(define inst-size 6)
(define inst-size-log (inexact->exact (ceiling (log inst-size 2))))
(define inst-size-bv (bv (sub1 inst-size) inst-size-log))
(define inst-useSt #f)
(define inst-len
  (+ inst-size-log param-reg-len param-rf-size-log param-rf-size-log))

(define inst-op-Li  (bv 0 inst-size-log))
(define inst-op-Add (bv 1 inst-size-log))
(define inst-op-Mul (bv 2 inst-size-log))
(define inst-op-Ld  (bv 3 inst-size-log))
(define inst-op-St  (bv 4 inst-size-log))
(define inst-op-Br  (bv 5 inst-size-log))


; PART inst
(struct inst (op rs1 rs2 rd)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (inst->string this) port))]
)


(define (init-inst op rs1 rs2 rd)
  (inst op rs1 rs2 rd)
)


(define (init-syminst)
  (define syminst
    (init-inst
      (build-symbv inst-size-log)
      (build-symbv param-reg-len)
      (build-symbv param-rf-size-log)
      (build-symbv param-rf-size-log)
    )
  )
  syminst
)


(define (bv->inst bv)
  (define inst (init-inst
    (extract (sub1 (+ inst-size-log param-reg-len
                      param-rf-size-log param-rf-size-log))
             (+ param-reg-len param-rf-size-log param-rf-size-log) bv)
    (extract (sub1 (+ param-reg-len param-rf-size-log param-rf-size-log))
             (+ param-rf-size-log param-rf-size-log) bv)
    (extract (sub1 (+ param-rf-size-log param-rf-size-log))
             (+ param-rf-size-log) bv)
    (extract (sub1 param-rf-size-log)
             0 bv)
  ))
  (assume-inst inst)
  inst
)


(define (assume-inst inst)
  (assume (bvule (inst-op inst) inst-size-bv))
  (when (not inst-useSt)
    (assume (not (bveq (inst-op inst) inst-op-St))))
)


(define (inst->string inst)
  (define rs1 (bitvector->natural (inst-rs1 inst)))
  (define rs1-rfIndex (bitvector->natural
    (extract (sub1 param-rf-size-log) 0 (inst-rs1 inst))))
  (define rs1-brOffset (bitvector->natural
    (extract (sub1 param-memi-size-log) 0 (inst-rs1 inst))))
  (define rs2 (bitvector->natural (inst-rs2 inst)))
  (define rd  (bitvector->natural (inst-rd  inst)))

  (cond
    [(term? (inst-op inst))
      (~a "(" (inst-op inst) " " (inst-rs1 inst) " "
          (inst-rs2 inst) " " (inst-rd inst) ")")
    ]
    [(bveq inst-op-Li (inst-op inst))
      (~a " Li " rs1 " " rs2 " " rd " : "
          "Reg[" rd "] <- " rs1)
    ]
    [(bveq inst-op-Add (inst-op inst))
      (~a "Add " rs1 " " rs2 " " rd " : "
          "Reg[" rd "] <- Reg[" rs1-rfIndex "] + Reg[" rs2 "]")
    ]
    [(bveq inst-op-Mul (inst-op inst))
      (~a "Mul " rs1 " " rs2 " " rd " : "
          "Reg[" rd "] <- Reg[" rs1-rfIndex "] * Reg[" rs2 "]")
    ]
    [(bveq inst-op-Ld  (inst-op inst))
      (~a " Ld " rs1 " " rs2 " " rd " : "
          "Reg[" rd "] <- Mem[Reg[" rs1-rfIndex "]]")
    ]
    [(bveq inst-op-St  (inst-op inst))
      (~a " St " rs1 " " rs2 " " rd " : "
          "Mem[Reg[" rs1-rfIndex "]] <- Reg[" rs2 "]")
    ]
    [(bveq inst-op-Br  (inst-op inst))
      (~a " Br " rs1 " " rs2 " " rd " : "
        "If (Reg[" rs2 "]==0) PC <- PC + " rs1-brOffset)
    ]
    [else
      (~a "unknown")]
  )
)


(define (inst-evaluate inst-sym sol)
  (init-inst (evaluate (inst-op  inst-sym) sol)
             (evaluate (inst-rs1 inst-sym) sol)
             (evaluate (inst-rs2 inst-sym) sol)
             (evaluate (inst-rd  inst-sym) sol))
)

