#lang rosette

(require
  "renameTB.rkt" "ROB.rkt"
  "../sym-state/rf.rkt" "../inst.rkt" "../lib/lib.rkt")
(provide rename decode)


; Given a regindex, this function return
;   - whether the youngest producer of this register has computed the result
;   - the result
(define (rename rs renameTB ROB rf)

  (define rs-ROBlink (array-ref (renameTB-ROBlink renameTB) rs))
  (define rs-in-ROB  (array-ref (renameTB-valid renameTB) rs))
  (define finish (array-ref (ROB-finished ROB) rs-ROBlink))

  (define stall #f) (define data (rf-ref rf rs))
  (when rs-in-ROB
    (if (array-ref (ROB-finished ROB) rs-ROBlink)
        (set! data (array-ref (ROB-rd-data ROB) rs-ROBlink))
        (set! stall #t)
    )
  )

  (list stall data)
)


(define (decode inst renameTB ROB rf)
  (define op (inst-op inst))
  (define rs1 (inst-rs1 inst))
  (define rs1-rfIndex (extract (sub1 param-rf-size-log) 0 rs1))
  (define rs2 (inst-rs2 inst))
  (define rd (inst-rd inst))

  (define rs1-imm rs1)
  (define rs1-brOffset (extract (sub1 param-memi-size-log) 0 rs1))
  (define rs1-ROBlink (array-ref (renameTB-ROBlink renameTB) rs1-rfIndex))
  (define rs2-ROBlink (array-ref (renameTB-ROBlink renameTB) rs2))

  (define rs1-stall #f)
  (define rs1-data rs1)
  (define rs2-stall #f)
  (define rs2-data (bv 0 param-reg-len))
  (define wen #f)
  (define mem-valid #f)
  (define mem-rdwt #f)
  (define isBr #f)

  (cond

    [(bveq inst-op-Li op)
      ;(set! rs1-stall #f)
      ;(set! rs1-data  rs1)
      ;(set! rs2-stall #f)

      (set! wen       #t)
      ;(set! mem-valid #f)
      ;(set! isBr      #f)
    ]
      
    [(or (bveq inst-op-Add op) (bveq inst-op-Mul op))
      (match-define (list stall_1 data_1) (rename rs1-rfIndex renameTB ROB rf))
      (set! rs1-stall stall_1)
      (set! rs1-data data_1)
      
      (match-define (list stall_2 data_2) (rename rs2 renameTB ROB rf))
      (set! rs2-stall stall_2)
      (set! rs2-data data_2)

      (set! wen       #t)
      ;(set! mem-valid #f)
      ;(set! isBr      #f)
    ]
      
    [(bveq inst-op-Ld op)
      (match-define (list stall_1 data_1) (rename rs1-rfIndex renameTB ROB rf))
      (set! rs1-stall stall_1)
      (set! rs1-data data_1)
      ;(set! rs2-stall #f)

      (set! wen       #t)
      (set! mem-valid #t)
      (set! mem-rdwt  #t)
      ;(set! isBr      #f)
    ]

    [(bveq inst-op-St op)
      (match-define (list stall_1 data_1) (rename rs1-rfIndex renameTB ROB rf))
      (set! rs1-stall stall_1)
      (set! rs1-data data_1)
      
      (match-define (list stall_2 data_2) (rename rs2 renameTB ROB rf))
      (set! rs2-stall stall_2)
      (set! rs2-data data_2)

      ;(set! wen       #f)
      (set! mem-valid #t)
      ;(set! mem-rdwt  #f)
      ;(set! isBr      #f)
    ]
      
    [(bveq inst-op-Br op)
      ;(set! rs1-stall #f)
      (match-define (list stall_2 data_2) (rename rs2 renameTB ROB rf))
      (set! rs2-stall stall_2)
      (set! rs2-data data_2)

      ;(set! wen       #f)
      ;(set! mem-valid #f)
      (set! isBr      #t)
    ]
  )


  (list rs1-imm rs1-brOffset rs1-stall rs1-data rs1-ROBlink
        rs2-stall rs2-data rs2-ROBlink
        wen rd
        mem-valid mem-rdwt
        isBr)
)

