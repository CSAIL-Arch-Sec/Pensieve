#lang rosette

(require "lib/lib.rkt" "sym-state/rf.rkt" "inst.rkt")
(provide decode)


; feed in functions
(define (decode inst pc rf memd)

  (define wen    #f)
  (define addr   (bv 0 param-rf-size-log))
  (define data   (bv 0 param-rf-size))
  (define brjmp  #f)
  (define taken  #f)
  (define target (bv 0 param-memi-size-log))
  (define mrd    #f)
  (define mwt    #f)
  (define maddr  (bv 0 param-memd-size-log))
  (define mdata  (bv 0 param-rf-size))

  (cond
    [(bveq inst-op-Li (inst-op inst))
      (set! wen #t)
      (set! addr (inst-rd inst))
      (set! data (inst-rs1 inst))
      (set! brjmp #f)
      (set! mrd #f)
      (set! mwt #f)
    ]
    
    [(bveq inst-op-Add (inst-op inst))
      (set! wen #t)
      (set! addr (inst-rd inst))
      (set! data (bvadd (rf-ref rf (extract (sub1 param-rf-size-log) 0 (inst-rs1 inst)))
                        (rf-ref rf (inst-rs2 inst))))
      (set! brjmp #f)
      (set! mrd #f)
      (set! mwt #f)
    ]
    
    [(bveq inst-op-Mul (inst-op inst))
      (set! wen #t)
      (set! addr (inst-rd inst))
      (set! data (bvmul (rf-ref rf (extract (sub1 param-rf-size-log) 0 (inst-rs1 inst)))
                        (rf-ref rf (inst-rs2 inst))))
      (set! brjmp #f)
      (set! mrd #f)
      (set! mwt #f)
    ]
    
    [(bveq inst-op-Ld (inst-op inst))
      (set! wen #t)
      (set! addr (inst-rd inst))
      (set! brjmp #f)
      (set! mrd #t)
      (set! maddr (extract (sub1 param-memd-size-log) 0
                           (rf-ref rf (extract (sub1 param-rf-size-log) 0 (inst-rs1 inst)))))
      (set! mwt #f)
    ]
    
    [(bveq inst-op-St (inst-op inst))
      (set! wen #f)
      (set! brjmp #f)
      (set! mrd #f)
      (set! mwt #t)
      (set! maddr (extract (sub1 param-memd-size-log) 0
                           (rf-ref rf (extract (sub1 param-rf-size-log) 0 (inst-rs1 inst)))))
      (set! mdata (rf-ref rf (inst-rs2 inst)))
    ]
    
    [(bveq inst-op-Br (inst-op inst))
      (set! wen #f)
      (set! brjmp #t)
      (set! taken (bvzero? (rf-ref rf (inst-rs2 inst))))
      (set! target (if taken
                       (bvadd (extract (sub1 param-memi-size-log) 0 (inst-rs1 inst)) pc)
                       (bvadd1 pc)))
      (set! mrd #f)
      (set! mwt #f)
    ]
  )

  (list wen     addr  data
        brjmp   taken target
        mrd mwt maddr mdata)
)

