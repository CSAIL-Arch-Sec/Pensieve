#lang rosette

(require
  "lib/lib.rkt" "sym-state/rf.rkt" "sym-state/memi.rkt" "sym-state/memd.rkt"
  "inst.rkt" "decode.rkt"
)
(provide
  (struct-out ISASimulator-cfg) init-ISASimulator-cfg-pair
  ISASimulator-cfg-evaluate ISASimulator-cfg-pair->string
  init-ISASimulator simu-ISASimulator!
  ISASimulator-memTrace assume-ISASimulator-memTrace
  ISASimulator-debug-commitLog
)


; PART ISASimulator-cfg
(struct ISASimulator-cfg (rf-cfg memi-cfg memd-cfg)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (ISASimulator-cfg->string this) port))]
)


(define (init-ISASimulator-cfg-pair)
  (define rf-cfg (init-rf-cfg param-rf-size param-reg-len param-rf-symType))
  (define memi-cfg (init-memi-cfg
    param-memi-size param-reg-len param-rf-size-log param-rf-size-log
    param-memi-symType))
  (match-define (list memd-cfg-0 memd-cfg-1) (init-memd-cfg-pair
    param-memd-size param-reg-len param-memd-symType))
  
  (list (ISASimulator-cfg rf-cfg memi-cfg memd-cfg-0)
        (ISASimulator-cfg rf-cfg memi-cfg memd-cfg-1))
)


(define (ISASimulator-cfg-evaluate ISASimulator-cfg-sym sol)
  (define rf-cfg (ISASimulator-cfg-rf-cfg ISASimulator-cfg-sym))
  (define memi-cfg (ISASimulator-cfg-memi-cfg ISASimulator-cfg-sym))
  (define memd-cfg (ISASimulator-cfg-memd-cfg ISASimulator-cfg-sym))

  (ISASimulator-cfg (rf-cfg-evaluate rf-cfg sol)
                    (memi-cfg-evaluate memi-cfg sol)
                    (memd-cfg-evaluate memd-cfg sol))
)


(define (ISASimulator-cfg->string ISASimulator-cfg)
  (define rf-cfg (ISASimulator-cfg-rf-cfg ISASimulator-cfg))
  (define memi-cfg (ISASimulator-cfg-memi-cfg ISASimulator-cfg))
  (define memd-cfg (ISASimulator-cfg-memd-cfg ISASimulator-cfg))

  (~a "memi: \n" memi-cfg "\n"
      "rf: " rf-cfg "\n"
      "memd: " memd-cfg "\n")
)


(define (ISASimulator-cfg-pair->string ISASimulator-cfg-0 ISASimulator-cfg-1)
  (define rf-cfg (ISASimulator-cfg-rf-cfg ISASimulator-cfg-0))
  (define memi-cfg (ISASimulator-cfg-memi-cfg ISASimulator-cfg-0))
  (define memd-cfg-0 (ISASimulator-cfg-memd-cfg ISASimulator-cfg-0))
  (define memd-cfg-1 (ISASimulator-cfg-memd-cfg ISASimulator-cfg-1))

  (~a "memi: \n" memi-cfg "\n"
      "rf: " rf-cfg "\n"
      "memd-0: " memd-cfg-0 " memd-1: " memd-cfg-1 "\n")
)


; PART ISASimulator
(struct ISASimulator (pc rf memi memd debug-commitLog param-simuCycle
                      param-resetsym-cycle param-debug-print-on)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (ISASimulator->string this) port))]
)


(define (init-ISASimulator ISASimulator-cfg param-simuCycle
                           param-resetsym-cycle param-debug-print-on)
  (define rf-cfg (ISASimulator-cfg-rf-cfg ISASimulator-cfg))
  (define memi-cfg (ISASimulator-cfg-memi-cfg ISASimulator-cfg))
  (define memd-cfg (ISASimulator-cfg-memd-cfg ISASimulator-cfg))
  
  (ISASimulator
    (bv 0 param-memi-size-log)
    (init-rf rf-cfg)
    (init-memi memi-cfg param-simuCycle)
    (init-memd memd-cfg param-simuCycle)
    (list)
    param-simuCycle
    param-resetsym-cycle
    param-debug-print-on)
)


(define (resetsym-ISASimulator! ISASimulator)
  (define rf (ISASimulator-rf ISASimulator))

  (resetsym-rf! rf)
)


(define (simu-ISASimulator! ISASimulator MAXCLK)
  (define pc (ISASimulator-pc ISASimulator))
  (define rf (ISASimulator-rf ISASimulator))
  (define memi (ISASimulator-memi ISASimulator))
  (define memd (ISASimulator-memd ISASimulator))
  (define debug-commitLog (ISASimulator-debug-commitLog ISASimulator))
  (define param-simuCycle (ISASimulator-param-simuCycle ISASimulator))
  (define param-resetsym-cycle (ISASimulator-param-resetsym-cycle ISASimulator))
  (define param-resetsym-cycle-bv
    (bv (sub1 param-resetsym-cycle)
        (inexact->exact (ceiling (log param-simuCycle 2)))))
  (define param-debug-print-on (ISASimulator-param-debug-print-on ISASimulator))

  ; STEP Fetch
  (define inst (logref-memi! memi pc))

  ; patch false counter example from SMT: branch target should not be next pc
  (when (bveq inst-op-Br (inst-op inst))
    (assume (not (bvzero? (bvsub1 (extract (sub1 param-memi-size-log) 0
                                           (inst-rs1 inst)))))))

  ; STEP Decode & Execute
  (match-define (list wen addr data brjmp taken target mrd mwt maddr mdata)
                (decode inst pc rf memd))

  ; STEP Memory
  (when mrd (set! data (logref-memd! memd maddr)))
  (when mwt (logset-memd! memd maddr mdata))

  ; STEP Debug verify correctness
  (when param-debug-commitLog-on
    (set-ISASimulator-debug-commitLog! ISASimulator (append
      debug-commitLog
      (if wen (list (list pc addr data)) (list (list pc))))))

  ; STEP Write Back
  (when wen (set-rf! rf addr data))
  (set-ISASimulator-pc! ISASimulator (if brjmp target (bvadd1 pc)))

  ; STEP tick
  (tick-memi! memi)
  (tick-memd! memd)

  ; STEP inc clk for mem
  (when (not (bvzero? MAXCLK))
    (when (and (not param-debug-print-on)
               (bvzero? (bvurem MAXCLK param-resetsym-cycle-bv)))
      (resetsym-ISASimulator! ISASimulator))
    (simu-ISASimulator! ISASimulator (bvsub1 MAXCLK)))
)


(define (ISASimulator->string ISASimulator)
  (define pc (ISASimulator-pc ISASimulator))
  (define rf (ISASimulator-rf ISASimulator))
  (define memi (ISASimulator-memi ISASimulator))
  (define memd (ISASimulator-memd ISASimulator))
  (define debug-commitLog (ISASimulator-debug-commitLog ISASimulator))

  (define debug-commitLog-int (map
    (lambda (entry) (map bitvector->natural entry))
    debug-commitLog))

  (~a
    ; "debug-commitLog: " debug-commitLog-int "\n"
    "memi: " memi "\n"
    "memd: " memd "\n"
  )
)


(define (ISASimulator-memTrace ISASimulator)
  (define memi (ISASimulator-memi ISASimulator))
  (define memd (ISASimulator-memd ISASimulator))

  (list (memi-history memi) (memd-history memd))
)


(define (assume-ISASimulator-memTrace memTrace-0 memTrace-1)
  (assume (equal? (first memTrace-0) (first memTrace-1)))
  (assume (equal? (second memTrace-0) (second memTrace-1)))
)


(define (testMe)
  (match-define (list ISASimulator-cfg-0 ISASimulator-cfg-1)
                (init-ISASimulator-cfg-pair))

  (define ISASimulator (init-ISASimulator
    ISASimulator-cfg-0
    param-ISASimulator-simuCycle param-ISASimulator-resetsym-cycle #t))

  (printf (~a ISASimulator "\n"))
  (simu-ISASimulator! ISASimulator param-ISASimulator-simuCycle-bv)
  (printf (~a ISASimulator "\n"))
  (printf (~a (ISASimulator-memTrace ISASimulator) "\n"))

)
;(testMe)

