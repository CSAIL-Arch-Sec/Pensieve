#lang rosette

(require
  "../lib/lib.rkt" "../inst.rkt")
(provide
  (struct-out ROB) init-ROB forward-ROB! ROB->brInfo clear-ROB-entry!
  squashPartial-ROB! resetsym-ROB! ROB-full ROB-empty
)


(struct ROB (
  idle waiting executing finished
  pc op
  rs1-imm rs1-brOffset rs1-stall rs1-data rs1-ROBlink
  rs2-stall rs2-data rs2-ROBlink
  wen rd rd-data
  mem-valid mem-rdwt mem-addr mem-data
  isBr brID taken pred-taken nextPC
  delayed-until-nonSpec underSpec specBr-ROBlink
  head tail)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (ROB->string this) port))]
)


(define (init-ROB)
  (ROB
    (init-array (make-vector param-ROB-size #t) param-ROB-size 1)
    (initFalseVec-array param-ROB-size)
    (initFalseVec-array param-ROB-size)
    (initFalseVec-array param-ROB-size)

    (initZeroVec-array param-ROB-size param-memi-size-log)
    (initZeroVec-array param-ROB-size inst-size-log)

    (initZeroVec-array param-ROB-size param-reg-len)
    (initZeroVec-array param-ROB-size param-memi-size-log)
    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-reg-len)
    (initZeroVec-array param-ROB-size param-ROB-size-log)
    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-reg-len)
    (initZeroVec-array param-ROB-size param-ROB-size-log)

    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-rf-size-log)
    (initZeroVec-array param-ROB-size param-reg-len)

    (initFalseVec-array param-ROB-size)
    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-memd-size-log)
    (initZeroVec-array param-ROB-size param-reg-len)

    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-brID-len)
    (initFalseVec-array param-ROB-size)
    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-memi-size-log)

    (initFalseVec-array param-ROB-size)
    (initFalseVec-array param-ROB-size)
    (initZeroVec-array param-ROB-size param-ROB-size-log)
    
    (bv 0 param-ROB-size-log)
    (bv 0 param-ROB-size-log))
)


; NOTE: Putting the rd-data as argument instead of reading it from ROB
;       is an optimization for symbolic execution
(define (forward-ROB! ROB pos rd-data)
  (for ([i (in-range param-ROB-size)])
    (define i-bv (integer->bitvector i (bitvector param-ROB-size-log)))
    (when (not (array-ref (ROB-idle ROB) i-bv))
      (when (and (array-ref (ROB-rs1-stall ROB) i-bv)
                 (bveq pos (array-ref (ROB-rs1-ROBlink ROB) i-bv)))
            (set-array! (ROB-rs1-stall ROB) i-bv #f)
            (set-array! (ROB-rs1-data ROB) i-bv rd-data))
      (when (and (array-ref (ROB-rs2-stall ROB) i-bv)
                 (bveq pos (array-ref (ROB-rs2-ROBlink ROB) i-bv)))
            (set-array! (ROB-rs2-stall ROB) i-bv #f)
            (set-array! (ROB-rs2-data ROB) i-bv rd-data))))
)


; TODO: we need to consider more info later
(define (ROB->brInfo ROB pos)
  (bool->bitvector (and (array-ref (ROB-isBr ROB) pos)
                        (array-ref (ROB-taken ROB) pos)))
)


(define (clear-ROB-entry! ROB pos)
  (set-array! (ROB-idle      ROB) pos #t)
  (set-array! (ROB-finished  ROB) pos #f)
  (set-array! (ROB-waiting   ROB) pos #f)
  (set-array! (ROB-executing ROB) pos #f)

  (set-array! (ROB-pc ROB) pos (bv 0 param-memi-size-log))
  (set-array! (ROB-op ROB) pos (bv 0 inst-size-log))

  (set-array! (ROB-rs1-imm      ROB) pos (bv 0 param-reg-len))
  (set-array! (ROB-rs1-brOffset ROB) pos (bv 0 param-memi-size-log))
  (set-array! (ROB-rs1-stall    ROB) pos #f)
  (set-array! (ROB-rs1-data     ROB) pos (bv 0 param-reg-len))
  (set-array! (ROB-rs1-ROBlink  ROB) pos (bv 0 param-ROB-size-log))
  (set-array! (ROB-rs2-stall    ROB) pos #f)
  (set-array! (ROB-rs2-data     ROB) pos (bv 0 param-reg-len))
  (set-array! (ROB-rs2-ROBlink  ROB) pos (bv 0 param-ROB-size-log))

  (set-array! (ROB-wen     ROB) pos #f)
  (set-array! (ROB-rd      ROB) pos (bv 0 param-rf-size-log))
  (set-array! (ROB-rd-data ROB) pos (bv 0 param-reg-len))

  (set-array! (ROB-mem-valid ROB) pos #f)
  (set-array! (ROB-mem-rdwt  ROB) pos #f)
  (set-array! (ROB-mem-addr  ROB) pos (bv 0 param-memd-size-log))
  (set-array! (ROB-mem-data  ROB) pos (bv 0 param-reg-len))

  (set-array! (ROB-isBr       ROB) pos #f)
  (set-array! (ROB-brID       ROB) pos (bv 0 param-brID-len))
  (set-array! (ROB-taken      ROB) pos #f)
  (set-array! (ROB-pred-taken ROB) pos #f)
  (set-array! (ROB-nextPC     ROB) pos (bv 0 param-memi-size-log))

  (set-array! (ROB-delayed-until-nonSpec ROB) pos #f)
  (set-array! (ROB-underSpec             ROB) pos #f)
  (set-array! (ROB-specBr-ROBlink        ROB) pos (bv 0 param-ROB-size-log))
)


(define (squashPartial-ROB! ROB misPredBr-ROBlink misPredBr-brID)

  (for ([i (in-range param-ROB-size)])
    (define i-bv (integer->bitvector i (bitvector param-ROB-size-log)))

    (define idle (array-ref (ROB-idle ROB) i-bv))
    (define brID (array-ref (ROB-brID ROB) i-bv))

    (when (bvugt brID misPredBr-brID)
      (clear-ROB-entry! ROB i-bv)))

  (set-ROB-tail! ROB (bvadd1 misPredBr-ROBlink))
)


(define (resetsym-ROB! ROB)
  (resetsym-boolVec-array! (ROB-idle      ROB))
  (resetsym-boolVec-array! (ROB-finished  ROB))
  (resetsym-boolVec-array! (ROB-waiting   ROB))
  (resetsym-boolVec-array! (ROB-executing ROB))

  (resetsym-bvVec-array! (ROB-pc ROB))
  (resetsym-bvVec-array! (ROB-op ROB))

  (resetsym-bvVec-array!   (ROB-rs1-imm      ROB))
  (resetsym-bvVec-array!   (ROB-rs1-brOffset ROB))
  (resetsym-boolVec-array! (ROB-rs1-stall    ROB))
  (resetsym-bvVec-array!   (ROB-rs1-data     ROB))
  (resetsym-bvVec-array!   (ROB-rs1-ROBlink  ROB))
  (resetsym-boolVec-array! (ROB-rs2-stall    ROB))
  (resetsym-bvVec-array!   (ROB-rs2-data     ROB))
  (resetsym-bvVec-array!   (ROB-rs2-ROBlink  ROB))

  (resetsym-boolVec-array! (ROB-wen     ROB))
  (resetsym-bvVec-array!   (ROB-rd      ROB))
  (resetsym-bvVec-array!   (ROB-rd-data ROB))

  (resetsym-boolVec-array! (ROB-mem-valid ROB))
  (resetsym-boolVec-array! (ROB-mem-rdwt  ROB))
  (resetsym-bvVec-array!   (ROB-mem-addr  ROB))
  (resetsym-bvVec-array!   (ROB-mem-data  ROB))

  (resetsym-boolVec-array! (ROB-isBr       ROB))
  (resetsym-bvVec-array!   (ROB-brID       ROB))
  (resetsym-boolVec-array! (ROB-taken      ROB))
  (resetsym-boolVec-array! (ROB-pred-taken ROB))
  (resetsym-bvVec-array!   (ROB-nextPC     ROB))

  (resetsym-boolVec-array! (ROB-delayed-until-nonSpec ROB))
  (resetsym-boolVec-array! (ROB-underSpec             ROB))
  (resetsym-bvVec-array!   (ROB-specBr-ROBlink        ROB))
)


(define (ROB-full ROB)
  (not (array-ref (ROB-idle ROB) (ROB-tail ROB)))
)


(define (ROB-empty ROB)
  (array-ref (ROB-idle ROB) (ROB-head ROB))
)


(define (ROB->string ROB)
  (~a "\n"
    "  head: " (bitvector->natural (ROB-head ROB)) ", "
    "tail: " (bitvector->natural (ROB-tail ROB)) "\n"

    "  idle: " (ROB-idle ROB) ", "
    "waiting: " (ROB-waiting ROB) "\n"
    
    "  executing: " (ROB-executing ROB) ", "
    "finished: " (ROB-finished ROB) "\n"

    "  pc: " (ROB-pc ROB) ", "
    "op: " (ROB-op ROB) ", "
    "brID: " (ROB-brID ROB) "\n"

    "  underSpec: " (ROB-underSpec ROB) ", "
    "specBr-ROBlink: " (ROB-specBr-ROBlink ROB) "\n"
  )
)

