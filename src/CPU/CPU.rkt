#lang rosette

(require
  "../lib/lib.rkt" "../inst.rkt"
  "../sym-state/rf.rkt" "../sym-state/memi.rkt" "../sym-state/memd.rkt"
  "../abs-module/brPred.rkt" "../abs-module/absFifo2.rkt"
  "../abs-module/absDelay.rkt" "alu.rkt" "cache.rkt" "ROB.rkt"
  "inFetchScoreBoard.rkt" "renameTB.rkt" "decode.rkt" "issue.rkt"
  "../ISASimulator.rkt"
)
(provide
  init-CPU-cfg-pair CPU-cfg-evaluate
  init-CPU simu-CPU! CPU-obsv assert-CPU-obsv
  CPU-debug-commitLog
)


; PART CPU-cfg
(struct CPU-cfg (rf-cfg memi-cfg memd-cfg Dcache-cfg adder-cfg muler-cfg
                 brPred-0-cfg brPred-1-cfg absDelay-cfg)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (CPU-cfg->string this) port))]
)


(define (init-CPU-cfg-pair ISASimulator-cfg-0 ISASimulator-cfg-1)
  (define rf-cfg (ISASimulator-cfg-rf-cfg ISASimulator-cfg-0))
  (define memi-cfg (ISASimulator-cfg-memi-cfg ISASimulator-cfg-0))
  (define memd-cfg-0 (ISASimulator-cfg-memd-cfg ISASimulator-cfg-0))
  (define memd-cfg-1 (ISASimulator-cfg-memd-cfg ISASimulator-cfg-1))

  (define adder-cfg (init-alu-cfg
    param-adder-timFct-len param-ROB-size param-CPU-simuCycle
    param-adder-symType))
  (define muler-cfg (init-alu-cfg
    param-muler-timFct-len param-ROB-size param-CPU-simuCycle
    param-muler-symType))
  (define Dcache-cfg (init-cache-cfg
    param-Dcache-timFct-len param-ROB-size param-CPU-simuCycle
    param-Dcache-symType))

  (define brPred-0-cfg (init-brPred-cfg
    param-brInfo-len param-CPU-simuCycle param-brPred-symType))
  (define brPred-1-cfg (init-brPred-cfg
    param-brInfo-len param-CPU-simuCycle param-brPred-symType))
  (define absDelay-cfg (init-absFifo2-cfg
    param-FDelay-timFct-len param-CPU-simuCycle param-FDelay-symType))
  
  (list (CPU-cfg rf-cfg memi-cfg memd-cfg-0 Dcache-cfg adder-cfg muler-cfg
                 brPred-0-cfg brPred-1-cfg absDelay-cfg)
        (CPU-cfg rf-cfg memi-cfg memd-cfg-1 Dcache-cfg adder-cfg muler-cfg
                 brPred-0-cfg brPred-1-cfg absDelay-cfg))
)


(define (CPU-cfg-evaluate CPU-cfg-sym sol)
  (define rf-cfg (CPU-cfg-rf-cfg CPU-cfg-sym))
  (define memi-cfg (CPU-cfg-memi-cfg CPU-cfg-sym))
  (define memd-cfg (CPU-cfg-memd-cfg CPU-cfg-sym))
  (define Dcache-cfg (CPU-cfg-Dcache-cfg CPU-cfg-sym))
  (define adder-cfg (CPU-cfg-adder-cfg CPU-cfg-sym))
  (define muler-cfg (CPU-cfg-muler-cfg CPU-cfg-sym))
  (define brPred-0-cfg (CPU-cfg-brPred-0-cfg CPU-cfg-sym))
  (define brPred-1-cfg (CPU-cfg-brPred-1-cfg CPU-cfg-sym))
  (define absDelay-cfg (CPU-cfg-absDelay-cfg CPU-cfg-sym))


  (CPU-cfg (rf-cfg-evaluate rf-cfg sol)
           (memi-cfg-evaluate memi-cfg sol)
           (memd-cfg-evaluate memd-cfg sol)
           (cache-cfg-evaluate Dcache-cfg sol)
           (alu-cfg-evaluate adder-cfg sol)
           (alu-cfg-evaluate muler-cfg sol)
           (brPred-cfg-evaluate brPred-0-cfg sol)
           (brPred-cfg-evaluate brPred-1-cfg sol)
           (absDelay-cfg-evaluate absDelay-cfg sol))
)


(define (CPU-cfg->string CPU-cfg)
  "CPU-cfg"
)


; PART CPU
(struct CPU (
  pc rf memi memd Dcache adder muler brID brPred-0 brPred-1 absDelay
  inFetchScoreBoard renameTB renameTB-copies ROB squash-C nextPC-C squash-E
  misPredBr-ROBlink-E misPredBr-brID-E nextPC-E underSpec fetchUnderSpec
  specBr-ROBlink commitPC debug-commitLog clk param-simuCycle
  param-resetsym-cycle param-debug-print-on)

  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (CPU->string this) port))]
)


(define (init-CPU CPU-cfg
                  param-simuCycle param-resetsym-cycle param-debug-print-on)
  (define rf-cfg (CPU-cfg-rf-cfg CPU-cfg))
  (define memi-cfg (CPU-cfg-memi-cfg CPU-cfg))
  (define memd-cfg (CPU-cfg-memd-cfg CPU-cfg))
  (define adder-cfg (CPU-cfg-adder-cfg CPU-cfg))
  (define muler-cfg (CPU-cfg-muler-cfg CPU-cfg))
  (define Dcache-cfg (CPU-cfg-Dcache-cfg CPU-cfg))
  (define brPred-0-cfg (CPU-cfg-brPred-0-cfg CPU-cfg))
  (define brPred-1-cfg (CPU-cfg-brPred-1-cfg CPU-cfg))
  (define absDelay-cfg (CPU-cfg-absDelay-cfg CPU-cfg))
  (define memd (init-memd memd-cfg param-simuCycle))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))
  
  (CPU
    (bv 0 param-memi-size-log)
    (init-rf rf-cfg)
    (init-memi memi-cfg param-simuCycle)
    memd
    (init-cache Dcache-cfg memd
                param-memd-size-log param-reg-len param-ROB-size-log)
    (init-alu adder-cfg bvadd param-reg-len param-ROB-size-log)
    (init-alu muler-cfg bvmul param-reg-len param-ROB-size-log)
    (bv 0 param-brID-len)
    (init-brPred brPred-0-cfg)
    (init-brPred brPred-1-cfg)
    (init-absDelay absDelay-cfg (+ param-memi-size-log 2))
    (init-inFetchScoreBoard param-simuCycle param-rf-size)
    (init-renameTB param-ROB-size param-rf-size)
    (init-renameTB-copies param-ROB-size param-rf-size param-ROB-size)
    (init-ROB)

    #f
    (bv 0 param-memi-size-log)
    #f
    (bv 0 param-ROB-size-log)
    (bv 0 param-brID-len)
    (bv 0 param-memi-size-log)

    #f
    #f
    (bv 0 param-ROB-size-log)

    (initZeroVec-array param-simuCycle param-memi-size-log)
    (list)
    (bv 0 param-simuCycle-log)

    param-simuCycle param-resetsym-cycle param-debug-print-on)
)


(define (resetsym-CPU! CPU)
  (resetsym-rf! (CPU-rf CPU))
  (resetsym-ROB! (CPU-ROB CPU))
)


(define (clearUnderSpec! ROB commitedBr-ROBlink)
  (for ([i (in-range param-ROB-size)])
    (define i-bv (integer->bitvector i (bitvector param-ROB-size-log)))
    (define specBr-ROBlink (array-ref (ROB-specBr-ROBlink ROB) i-bv))

    (when (bveq commitedBr-ROBlink specBr-ROBlink)
      (set-array! (ROB-underSpec ROB) i-bv #f)))
)


(define (evalC! CPU)
  (define rf (CPU-rf CPU))
  (define memd (CPU-memd CPU))
  (define Dcache (CPU-Dcache CPU))
  (define brPred-0 (CPU-brPred-0 CPU))
  (define brPred-1 (CPU-brPred-1 CPU))
  (define renameTB (CPU-renameTB CPU))
  (define ROB (CPU-ROB CPU))
  (define specBr-ROBlink (CPU-specBr-ROBlink CPU))
  (define commitPC (CPU-commitPC CPU))
  (define debug-commitLog (CPU-debug-commitLog CPU))
  (define clk (CPU-clk CPU))

  (define head (ROB-head ROB))

  (set-CPU-squash-C! CPU #f)

  (when (array-ref (ROB-finished ROB) head)

    (set-array! commitPC clk (array-ref (ROB-pc ROB) head))
    ; STEP: Debug verify correctness
    (when param-debug-commitLog-on
      (set-CPU-debug-commitLog! CPU (append debug-commitLog
        (if (array-ref (ROB-wen ROB) head)
            (list (list (array-ref (ROB-pc      ROB) head)
                        (array-ref (ROB-rd      ROB) head)
                        (array-ref (ROB-rd-data ROB) head)))
            (list (list (array-ref (ROB-pc      ROB) head)))))))


    ; STEP: Deal with rf write
    (when (array-ref (ROB-wen ROB) head)
      ; STEP-1: write to register file
      (set-rf! rf (array-ref (ROB-rd ROB) head)
                  (array-ref (ROB-rd-data ROB) head))

      ; STEP-2: release myself from rename table
      (define wen (array-ref (ROB-wen ROB) head))
      (define rd (array-ref (ROB-rd ROB) head))
      (define valid-array (renameTB-valid renameTB))
      (define ROBlink-array (renameTB-ROBlink renameTB))

      (when wen
        (when param-debug-assert (bug-assert
          (array-ref valid-array rd)
          #:msg (lambda (sol) (format
            "renameTB release invalid: \nrenameTB: ~v \nrd: ~v"
            (evaluate renameTB sol) (evaluate rd sol)))))

        (when (bveq (array-ref ROBlink-array rd) head)
          (set-array! valid-array   rd #f)
          (set-array! ROBlink-array rd (bv 0 param-ROB-size-log)))))


    ; STEP: Deal with branch
    (when (array-ref (ROB-isBr ROB) head)

      ; STEP-1-1: update the underSpec status
      (when (bveq specBr-ROBlink head) (set-CPU-underSpec! CPU #f))

      ; STEP-1-2: forward to clear the is-speculated bit
      (clearUnderSpec! ROB head)

      ; STEP-2: update brInfo
      (feedBrInfo-brPred! brPred-0 (ROB->brInfo ROB head))
      (feedBrInfo-brPred! brPred-1 (ROB->brInfo ROB head))

      ; STEP-3: Mis-predicted squash
      (when (not (equal? (array-ref (ROB-pred-taken ROB) head)
                         (array-ref (ROB-taken      ROB) head)))
            (set-CPU-squash-C! CPU #t))
      (set-CPU-nextPC-C! CPU (array-ref (ROB-nextPC ROB) head)))


    ; STEP: Deal with mem write
    ; TODO: use cache
    (when (and (array-ref (ROB-mem-valid ROB) head)
               (not (array-ref (ROB-mem-rdwt ROB) head)))
      (logset-memd! memd
                    (extract (sub1 param-memd-size-log) 0
                             (array-ref (ROB-rs1-data ROB) head))
                    (array-ref (ROB-rs2-data ROB) head)))
    
    ; STEP5: Finish committing this inst
    (clear-ROB-entry! ROB head)
    (set-ROB-head! ROB (bvadd1 head)))
)


(define (evalE! CPU)
  (define memd (CPU-memd CPU))
  (define adder (CPU-adder CPU))
  (define muler (CPU-muler CPU))
  (define Dcache (CPU-Dcache CPU))
  (define ROB (CPU-ROB CPU))
  (define param-debug-print-on (CPU-param-debug-print-on CPU))

  (match-define (list squash-E misPredBr-ROBlink-E misPredBr-brID-E nextPC-E)
                (issue! memd adder muler Dcache ROB param-debug-print-on))
  (set-CPU-squash-E! CPU squash-E)
  ; (when param-debug-print-on (printf (~a
  ;   "squash-E: " squash-E "  "
  ;   "misPredBr-ROBlink-E: " misPredBr-ROBlink-E "  "
  ;   "misPredBr-brID-E: " misPredBr-brID-E "  "
  ;   "nextPC-E: " nextPC-E "\n"
  ; )))
  (set-CPU-misPredBr-ROBlink-E! CPU misPredBr-ROBlink-E)
  (set-CPU-misPredBr-brID-E! CPU misPredBr-brID-E)
  (set-CPU-nextPC-E! CPU nextPC-E)
)


(define (squash! CPU)
  (define Dcache (CPU-Dcache CPU))
  (define adder (CPU-adder CPU))
  (define muler (CPU-muler CPU))
  (define absDelay (CPU-absDelay CPU))
  (define inFetchScoreBoard (CPU-inFetchScoreBoard CPU))
  (define renameTB-copies (CPU-renameTB-copies CPU))
  (define ROB (CPU-ROB CPU))
  (define squash-C (CPU-squash-C CPU))
  (define nextPC-C (CPU-nextPC-C CPU))
  (define squash-E (CPU-squash-E CPU))
  (define misPredBr-ROBlink-E (CPU-misPredBr-ROBlink-E CPU))
  (define misPredBr-brID-E (CPU-misPredBr-brID-E CPU))
  (define nextPC-E (CPU-nextPC-E CPU))
  (define param-simuCycle (CPU-param-simuCycle CPU))

  (when squash-E
    (set-CPU-pc! CPU nextPC-E)
    (squashPartial-cache! Dcache misPredBr-brID-E)
    (squashPartial-alu! adder misPredBr-brID-E)
    (squashPartial-alu! muler misPredBr-brID-E)
    (drainSquash-absDelay! absDelay)
    (squashPartial-ROB! ROB misPredBr-ROBlink-E misPredBr-brID-E)
    (set-CPU-inFetchScoreBoard! CPU
      (init-inFetchScoreBoard param-simuCycle param-rf-size))
    ; NOTE: we do not need to use the renameTB-copies
    ;       that is updated at this cycle
    (set-CPU-renameTB! CPU (vector-ref-bv renameTB-copies misPredBr-ROBlink-E))

    (define underSpec-recovered
      (array-ref (ROB-underSpec ROB) misPredBr-ROBlink-E))
    (define specBr-ROBlink-recovered
      (array-ref (ROB-specBr-ROBlink ROB) misPredBr-ROBlink-E))
    (set-CPU-underSpec! CPU underSpec-recovered)
    (set-CPU-fetchUnderSpec! CPU underSpec-recovered)
    (set-CPU-specBr-ROBlink! CPU specBr-ROBlink-recovered))

  (when squash-C
    (set-CPU-pc! CPU nextPC-C)
    (squash-cache! Dcache)
    (squash-alu! adder)
    (squash-alu! muler)
    (drainSquash-absDelay! (CPU-absDelay CPU))
    (set-CPU-ROB! CPU (init-ROB))
    (set-CPU-inFetchScoreBoard! CPU
      (init-inFetchScoreBoard param-simuCycle param-rf-size))
    (set-CPU-renameTB! CPU (init-renameTB param-ROB-size param-rf-size))
    (set-CPU-underSpec! CPU #f)
    (set-CPU-fetchUnderSpec! CPU #f))

)


(define (evalF! CPU)
  (define rf (CPU-rf CPU))
  (define memi (CPU-memi CPU))
  (define brPred-0 (CPU-brPred-0 CPU))
  (define brPred-1 (CPU-brPred-1 CPU))
  (define absDelay (CPU-absDelay CPU))
  (define inFetchScoreBoard (CPU-inFetchScoreBoard CPU))
  (define numProducer-array (inFetchScoreBoard-numProducer inFetchScoreBoard))
  (define renameTB-i (CPU-renameTB CPU))
  (define renameTB-copies (CPU-renameTB-copies CPU))
  (define ROB (CPU-ROB CPU))
  (define param-debug-print-on (CPU-param-debug-print-on CPU))

  ; STEP: helper to use absDelay
  (define (encoder pc pred-taken isSpec)
    (concat pc (bool->bitvector pred-taken) (bool->bitvector isSpec)))
  (define (decoder entry)
    (define pc (extract (+ 1 param-memi-size-log) 2 entry))
    (define pred-taken (bitvector->bool (extract 1 1 entry)))
    (define isSpec (bitvector->bool (extract 0 0 entry)))
    (list pc pred-taken isSpec))


  ; STEP: pop from absDelay and push to ROB
  (define (push-one-to-ROB!)
    (define brID (CPU-brID CPU))
    (define underSpec (CPU-underSpec CPU))
    (define specBr-ROBlink (CPU-specBr-ROBlink CPU))

    ; STEP-1: pop from absDelay
    (match-define (list pc pred-taken isSpec)
                  (decoder (dataout-absDelay! absDelay)))
    (define inst (memi-ref memi pc))

    ; STEP-2: decode the instruction
    (match-define (list rs1-imm rs1-brOffset rs1-stall rs1-data rs1-ROBlink
                        rs2-stall rs2-data rs2-ROBlink
                        wen rd
                        mem-valid mem-rdwt
                        isBr)
                  (decode inst renameTB-i ROB rf))

    ; STEP-3: push into ROB
    (define tail (ROB-tail ROB))
    (set-array! (ROB-idle    ROB) tail #f)
    (set-array! (ROB-waiting ROB) tail #t)
    (set-array! (ROB-pc      ROB) tail pc)
    (set-array! (ROB-op      ROB) tail (inst-op inst))

    (set-array! (ROB-rs1-imm      ROB) tail rs1-imm)
    (set-array! (ROB-rs1-brOffset ROB) tail rs1-brOffset)
    (set-array! (ROB-rs1-stall    ROB) tail rs1-stall)
    (set-array! (ROB-rs1-data     ROB) tail rs1-data)
    (set-array! (ROB-rs1-ROBlink  ROB) tail rs1-ROBlink)
    (set-array! (ROB-rs2-stall    ROB) tail rs2-stall)
    (set-array! (ROB-rs2-data     ROB) tail rs2-data)
    (set-array! (ROB-rs2-ROBlink  ROB) tail rs2-ROBlink)
    
    (set-array! (ROB-wen ROB) tail wen)
    (set-array! (ROB-rd  ROB) tail rd)
    
    (set-array! (ROB-mem-valid ROB) tail mem-valid)
    (set-array! (ROB-mem-rdwt  ROB) tail mem-rdwt)
    
    (set-array! (ROB-isBr       ROB) tail isBr)
    (set-array! (ROB-brID       ROB) tail brID)
    (set-array! (ROB-pred-taken ROB) tail pred-taken)
    
    (set-array! (ROB-underSpec      ROB) tail underSpec)
    (set-array! (ROB-specBr-ROBlink ROB) tail specBr-ROBlink)

    (set-ROB-tail! ROB (bvadd1 tail))

    ; STEP-4: inc brID
    (set-CPU-brID! CPU (bvadd1 brID))

    ; STEP-5: update the renaming table
    (when wen
      (define rd (inst-rd inst))
      (define numProducer (array-ref numProducer-array rd))
      (set-array! numProducer-array rd (bvsub1 numProducer))

      (set-array! (renameTB-valid   renameTB-i) rd #t)
      (set-array! (renameTB-ROBlink renameTB-i) rd tail))
    (vector-set!-bv renameTB-copies tail
      (renameTB (initFromCopy-array (renameTB-valid   renameTB-i))
                (initFromCopy-array (renameTB-ROBlink renameTB-i))))

    ; STEP-6: book keep whether is a speculated one
    (when (and isBr isSpec)
      (set-CPU-underSpec!      CPU #t)
      (set-CPU-specBr-ROBlink! CPU tail))

    ; (when param-debug-print-on (printf (~a
    ;   "---->" "pc: " pc "  "
    ;           "underSpec: " isSpec "  "
    ;           "specBr-ROBlink: " tail "\n"
    ; )))
  )

  (match-define (list valid-0 valid-1) (absDelay-dataoutValid absDelay))
  (when (and valid-0 (not (ROB-full ROB))) (push-one-to-ROB!))
  (when (and valid-1 (not (ROB-full ROB))) (push-one-to-ROB!))


  ; STEP: push to absDelay
  (define (push-one-to-absDelay! brPred)
    (define pc (CPU-pc CPU))
    (define fetchUnderSpec (CPU-fetchUnderSpec CPU))
    (define inst (logref-memi! memi pc))
    (define op (inst-op inst))
    (define rs1 (inst-rs1 inst))
    (define rs2 (inst-rs2 inst))
    (define rd (inst-rd inst))

    ; STEP-1: update inFetchScoreBoard
    (when (or (bveq inst-op-Li op) (bveq inst-op-Add op)
              (bveq inst-op-Mul op) (bveq inst-op-Ld op))
      (define numProducer (array-ref numProducer-array rd))
      (set-array! numProducer-array rd (bvadd1 numProducer)))

    (cond
      ; STEP-2: if is branch inst
      [(bveq inst-op-Br op)
        ; NOTE: only use commited data in rf to decide branch direction
        (define rs2-stall (or
          (array-ref (renameTB-valid renameTB-i) rs2)
          (not (bvzero? (array-ref numProducer-array rs2)))
          fetchUnderSpec))
        (define rs2-data (rf-ref rf rs2))

        ; NOTE: we need this because
        ;       we want fetched pc only depend on spec data.
        ;       However, if we use the spec fetch branch's rs2
        ;       to decide spec or not,
        ;       this is already using spec data to fetch next pc
        ; TODO: now fetchUnderSpec is only unset at squash.
        ;       Need to be more precise.
        (when rs2-stall (set-CPU-fetchUnderSpec! CPU #t))

        (define taken (if rs2-stall
                          (brPred-predict brPred)
                          (bvzero? rs2-data)))
        (define target (bvadd (extract (sub1 param-memi-size-log) 0 rs1)
                              pc))
        (set-CPU-pc! CPU (if taken
                             target
                             (bvadd1 pc)))

        ; (when param-debug-print-on (printf (~a
        ;   "---->" "pc: " pc "  " "underSpec: " rs2-stall "\n"
        ; )))
        (list pc taken rs2-stall)
      ]

      ; STEP-3: if is normal inst
      [else
        (set-CPU-pc! CPU (bvadd1 pc))
        (list pc #f #f)]))

  (match-define (list pc-0 pred-taken-0 rs2-stall-0)
                (push-one-to-absDelay! brPred-0))
  (match-define (list pc-1 pred-taken-1 rs2-stall-1)
                (push-one-to-absDelay! brPred-1))
  ; TODO: maybe add the brInfo to timFct
  (datain-absDelay! absDelay
                    (encoder pc-0 pred-taken-0 rs2-stall-0) pc-0
                    (encoder pc-1 pred-taken-1 rs2-stall-1) pc-1)
)


; TODO: move this between C and E?
(define (check-return! CPU)
  (define adder (CPU-adder CPU))
  (define muler (CPU-muler CPU))
  (define Dcache (CPU-Dcache CPU))
  (define ROB (CPU-ROB CPU))
  (define param-debug-print-on (CPU-param-debug-print-on CPU))

  ; STEP: check the returns from alu and cache
  (define (check-return-normal! unit unit-dataoutValid dataout-unit!)
    (when (unit-dataoutValid unit)

      ; STEP-1: get response packet
      (match-define (list ROBlink result) (dataout-unit! unit))

      ; (when param-debug-print-on (printf (~a
      ;   "return ROBlink: " ROBlink "\n"
      ; )))
      (when param-debug-assert (bug-assert
        (array-ref (ROB-executing ROB) ROBlink)
        #:msg "adder/muler/Dcache return to an un-executing ROB entry"))

      ; STEP-2: fill in the ROB entry
      (set-array! (ROB-executing ROB) ROBlink #f)
      (set-array! (ROB-finished ROB) ROBlink #t)
      (set-array! (ROB-rd-data ROB) ROBlink result)

      ; STEP-3: forward the result
      (forward-ROB! ROB ROBlink result)))

  (check-return-normal! adder alu-dataoutValid dataout-alu!)
  (check-return-normal! muler alu-dataoutValid dataout-alu!)
  (check-return-normal! Dcache cache-dataoutValid dataout-cache!)


  ; STEP: check the hit from cache
  (when param-cache-useHit
    (match-define (list hitValid ROBlink result) (checkHitReq-cache! Dcache))
    (when hitValid
      (set-array! (ROB-executing ROB) ROBlink #f)
      (set-array! (ROB-finished ROB) ROBlink #t)
      (set-array! (ROB-rd-data ROB) ROBlink result)
      (forward-ROB! ROB ROBlink result)))


  ; STEP: check the delay-on-miss from cache
  (when param-enable-DoM
    (match-define (list delayValid ROBlink) (checkDelayedReq-cache! Dcache))
    (when delayValid
      (set-array! (ROB-waiting ROB) ROBlink #t)
      (set-array! (ROB-executing ROB) ROBlink #f)
      (set-array! (ROB-delayed-until-nonSpec ROB) ROBlink #t)))
)


(define (simu-CPU! CPU MAXCLK)

  (define memi  (CPU-memi  CPU))
  (define Dcache (CPU-Dcache CPU))
  (define adder (CPU-adder CPU))
  (define muler (CPU-muler CPU))
  (define brPred-0 (CPU-brPred-0 CPU))
  (define brPred-1 (CPU-brPred-1 CPU))
  (define absDelay (CPU-absDelay CPU))
  (define clk (CPU-clk CPU))
  (define param-simuCycle (CPU-param-simuCycle CPU))
  (define param-resetsym-cycle (CPU-param-resetsym-cycle CPU))
  (define param-resetsym-cycle-bv
    (bv (sub1 param-resetsym-cycle)
        (inexact->exact (ceiling (log param-simuCycle 2)))))
  (define param-debug-print-on (CPU-param-debug-print-on CPU))

  (when param-debug-print-on (printf (~a
    "******CPU at cycle " (bitvector->natural clk) "******\n" CPU "\n"
  )))

  (evalC! CPU)
  (evalE! CPU)
  (squash! CPU)
  (evalF! CPU)

  ; (when param-debug-print-on (printf (~a
  ;   "CPU:\n" CPU "\n"
  ; )))

  (tick-memi! memi)
  (tick-cache! Dcache)
  (tick-alu! adder)
  (tick-alu! muler)
  (tick-brPred! brPred-0)
  (tick-brPred! brPred-1)
  (tick-absDelay! absDelay)
  (set-CPU-clk! CPU (bvadd1 clk))

  (check-return! CPU)

  (when (not (bvzero? MAXCLK))
    (when (and (not param-debug-print-on)
               (bvzero? (bvurem MAXCLK param-resetsym-cycle-bv)))
      (resetsym-CPU! CPU))
    (simu-CPU! CPU (bvsub1 MAXCLK)))
)


(define (CPU->string CPU)
  (define memi (CPU-memi CPU))
  (define memd (CPU-memd CPU))
  (define Dcache (CPU-Dcache CPU))
  (define adder (CPU-adder CPU))
  (define ROB (CPU-ROB CPU))
  (define underSpec (CPU-underSpec CPU))
  (define specBr-ROBlink (CPU-specBr-ROBlink CPU))
  (define debug-commitLog (CPU-debug-commitLog CPU))

  (define debug-commitLog-int (map
    (lambda (entry) (map bitvector->natural entry))
    debug-commitLog))
  (~a
    ; "debug-commitLog: " debug-commitLog-int "  "
    "underSpec: " underSpec "  " "specBr-ROBlink" specBr-ROBlink "\n"
    "memi: " memi "\n"
    "Dcache: " Dcache "\n"
    "adder: " adder "\n"
    "ROB: " ROB "\n"
  )
)


(define (CPU-obsv CPU)
  (cond
    [(or (equal? param-obsvType "memTrace")
         (equal? param-obsvType "cacheState"))
      (define Dcache (CPU-Dcache CPU))
      (cache-obsv Dcache)]

    [(equal? param-obsvType "commitPC")
      (define commitPC (CPU-commitPC CPU))
      (array->bv commitPC)]
  )
)


(define (assert-CPU-obsv obsv-0 obsv-1)
  (assert (equal? obsv-0 obsv-1))
)


(define (assert-CPU-debug-commitLog commitLog-0 commitLog-1)
  (assert (equal? commitLog-0 commitLog-1))
)


(define (testMe)

  (match-define (list ISASimulator-cfg-0 ISASimulator-cfg-1)
                (init-ISASimulator-cfg-pair))
  (match-define (list CPU-cfg-0 CPU-cfg-1)
                (init-CPU-cfg-pair ISASimulator-cfg-0 ISASimulator-cfg-1))


  (define CPU (init-CPU
    CPU-cfg-0
    param-CPU-simuCycle param-CPU-resetsym-cycle #t))


  (simu-CPU! CPU param-CPU-simuCycle-bv)
  (println CPU)

)
;(testMe)

