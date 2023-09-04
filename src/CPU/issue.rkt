#lang rosette

(require
  "ROB.rkt" "alu.rkt" "cache.rkt" "../sym-state/memd.rkt" "../inst.rkt"
  "../lib/lib.rkt"
)
(provide issue!)


(define (issue! memd adder muler Dcache ROB param-debug-print-on)
  ; TODO: check memory st->ld forward before issuing with stld alias table

  ; STEP: we remember a forwardlist to deal with after issuing everything
  ;       to avoid the forward data being used in the same cycle
  (define forwardlist (list))

  (define squash-E #f)
  (define misPredBr-ROBlink-E (bv 0 param-ROB-size-log))
  (define misPredBr-brID-E (bv -1 param-brID-len))
  (define nextPC-E (bv 0 param-memi-size-log))

  (for ([i (in-range param-ROB-size)])
    (define i-bv (integer->bitvector i (bitvector param-ROB-size-log)))

    (define waiting (array-ref (ROB-waiting ROB) i-bv))
    (define waiting-array (ROB-waiting ROB))
    (define executing-array (ROB-executing ROB))
    (define finished-array (ROB-finished ROB))

    (define pc (array-ref (ROB-pc ROB) i-bv))
    (define op (array-ref (ROB-op ROB) i-bv))

    (define rs1-imm (array-ref (ROB-rs1-imm ROB) i-bv))
    (define rs1-brOffset (array-ref (ROB-rs1-brOffset ROB) i-bv))
    (define rs1-stall (array-ref (ROB-rs1-stall ROB) i-bv))
    (define rs1-data (array-ref (ROB-rs1-data ROB) i-bv))
    (define rs2-stall (array-ref (ROB-rs2-stall ROB) i-bv))
    (define rs2-data (array-ref (ROB-rs2-data ROB) i-bv))

    (define rd (array-ref (ROB-rd ROB) i-bv))
    (define rd-data-array (ROB-rd-data ROB))

    (define mem-rdwt (array-ref (ROB-mem-rdwt ROB) i-bv))
    (define mem-addr-array (ROB-mem-addr ROB))
    (define mem-data-array (ROB-mem-data ROB))

    (define brID (array-ref (ROB-brID ROB) i-bv))
    (define taken-array (ROB-taken ROB))
    (define pred-taken-array (ROB-pred-taken ROB))
    (define pred-taken (array-ref (ROB-pred-taken ROB) i-bv))
    (define nextPC-array (ROB-nextPC ROB))

    (define delayed-until-nonSpec
      (array-ref (ROB-delayed-until-nonSpec ROB) i-bv))
    (define underSpec (array-ref (ROB-underSpec ROB) i-bv))


    ; STEP: the condition to issue
    (when (and waiting (not rs1-stall) (not rs2-stall))

      ; STEP: execute the instruction
      (cond
        [(bveq inst-op-Li op)
          (set-array! rd-data-array i-bv rs1-imm)
          (set-array! waiting-array i-bv #f)
          (set-array! finished-array i-bv #t)
          (set-array! nextPC-array i-bv (bvadd1 pc))
          (set! forwardlist (append forwardlist (list (list i-bv rs1-imm))))]

        [(bveq inst-op-Add op)
          (datain-alu! adder brID i-bv rs1-data rs2-data i-bv (bv 0 1))
          (set-array! nextPC-array i-bv (bvadd1 pc))
        ]

        [(bveq inst-op-Mul op)
          (datain-alu! muler brID i-bv rs1-data rs2-data i-bv (bv 0 1))
          (set-array! nextPC-array i-bv (bvadd1 pc))
        ]

        [(and (bveq inst-op-Ld op) (not (and delayed-until-nonSpec underSpec)))
          (define mem-addr (extract (sub1 param-memd-size-log) 0 rs1-data))
          (define mem-data rs2-data)
          (set-array! mem-addr-array i-bv mem-addr)
          (datain-cache! Dcache underSpec brID i-bv mem-addr mem-data mem-rdwt
                         i-bv mem-addr)
          (set-array! nextPC-array i-bv (bvadd1 pc))
        ]

        [(bveq inst-op-St op)
          (define mem-addr (extract (sub1 param-memd-size-log) 0 rs1-data))
          (define mem-data rs2-data)
          (set-array! mem-addr-array i-bv mem-addr)
          (set-array! mem-data-array i-bv mem-data)
          (set-array! waiting-array i-bv #f)
          (set-array! finished-array i-bv #t)
          (set-array! nextPC-array i-bv (bvadd1 pc))]

        [(bveq inst-op-Br op)
          (define taken (bvzero? rs2-data))
          (define nextPC (if (bvzero? rs2-data) (bvadd rs1-brOffset pc) (bvadd1 pc)))
          (set-array! taken-array i-bv taken)
          (set-array! nextPC-array i-bv nextPC)

          (when param-enable-execute-squash
            (when (not (equal? pred-taken taken))
              (set! squash-E #t)
              (when (bvult brID misPredBr-brID-E)
                (set! misPredBr-ROBlink-E i-bv)
                (set! misPredBr-brID-E brID)
                (set! nextPC-E nextPC))
              (set-array! pred-taken-array i-bv taken)))

          (set-array! waiting-array i-bv #f)
          (set-array! finished-array i-bv #t)])))


  ; STEP: check the datainReady from alu and cache
  (define (check-datainReady! port port-datainReady)
    ; STEP-1: get datainReady packet
    (match-define (list datainReady ROBlink) (port-datainReady port))
    ; (when param-debug-print-on (printf (~a
    ;   "datainReady: " datainReady "  " "ROBlink: " ROBlink "\n"
    ; )))
    (when datainReady
      (when param-debug-assert (bug-assert
        (array-ref (ROB-waiting ROB) ROBlink)
        #:msg "adder/muler/Dcache ready to an un-waiting ROB entry"))

      ; STEP-2: fill in the ROB entry
      (set-array! (ROB-waiting ROB) ROBlink #f)
      (set-array! (ROB-executing ROB) ROBlink #t)))
  
  (check-datainReady! adder alu-datainReady)
  (check-datainReady! muler alu-datainReady)
  (check-datainReady! Dcache cache-datainReady)


  ; STEP: Forward the data from finished ROB
  (for-each
    (lambda (entry) (forward-ROB! ROB (first entry) (second entry)))
    forwardlist)

  (list squash-E misPredBr-ROBlink-E misPredBr-brID-E nextPC-E)
)

