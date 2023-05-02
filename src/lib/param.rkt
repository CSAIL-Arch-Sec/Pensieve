#lang rosette

(require rosette/solver/smt/z3 rosette/solver/smt/boolector)
(provide (all-defined-out))


; PART symbolic value type
(define param-rf-symType "vec_sym") ; vec_concrete, vec_sym,
                                    ; func_concrete, func_sym
(define param-memi-symType "func_sym") ; vec_concrete, vec_sym, func_sym
(define param-memd-symType "func_sym") ; vec_concrete, vec_sym, func_sym

(define param-adder-symType "func_sym") ; func_concrete, func_sym
(define param-muler-symType "func_sym") ; func_concrete, func_sym
(define param-Dcache-symType "func_sym") ; func_concrete, func_sym

(define param-brPred-symType "func_sym") ; func_concrete, func_sym
(define param-FDelay-symType "func_sym") ; func_concrete, func_sym



; PART Architecture
(define param-rf-size 4)
(define param-reg-len 4)
(define param-memi-size 16)
(define param-memd-size 4)




; PART Micro-architecture
(define param-ROB-size 16)
(define param-brInfo-len 1)
(define param-enable-execute-squash #f)
(define param-cache-useHit #f)




; PART: Observation Model
(define param-obsvType "memTrace") ; memTrace, cacheState, commitPC




; PART: Defenses
(define param-enable-DoM #f)
(define param-enable-invisiSpec #f)
(define param-enable-arbiterPriority #f)
(define param-enable-GhostMinion #f)




; STEP Timing Factor
; TODO: this should be zero
(define param-adder-timFct-len 1)
(define param-muler-timFct-len 1)




; STEP Simulation
(define param-CPU-simuCycle 4) ; 4 for spectre



; PART Symbolic Optimization
(define param-ISASimulator-resetsym-cycle 16)
(define param-CPU-resetsym-cycle 16)

(current-solver (boolector)) ; z3, boolector



; STEP Debug
(define param-debug-commitLog-on #t)
(define param-debug-assert #t)
(error-print-width 1000)




; STEP Saved params here
(define param-saved-params "None")
(define param-saved-sizes "None")




; STEP Override from command line
(command-line
  #:once-each
  [("--param-debug-assert") v "param-debug-assert"
    (set! param-debug-assert (equal? 1 (string->number v)))]

  [("--param-enable-execute-squash") v "param-enable-execute-squash"
    (set! param-enable-execute-squash (equal? 1 (string->number v)))]

  [("--param-obsvType") v "param-obsvType"
    (set! param-obsvType v)]

  [("--param-cache-useHit") v "param-cache-useHit"
    (set! param-cache-useHit (equal? 1 (string->number v)))]
  [("--param-enable-DoM") v "param-enable-DoM"
    (set! param-enable-DoM (equal? 1 (string->number v)))]
  [("--param-enable-invisiSpec") v "param-enable-invisiSpec"
    (set! param-enable-invisiSpec (equal? 1 (string->number v)))]
  [("--param-enable-arbiterPriority") v "param-enable-arbiterPriority"
    (set! param-enable-arbiterPriority (equal? 1 (string->number v)))]
  [("--param-enable-GhostMinion") v "param-enable-GhostMinion"
    (set! param-enable-GhostMinion (equal? 1 (string->number v)))]

  [("--param-ROB-size") v "param-ROB-size"
    (set! param-ROB-size (string->number v))]
  [("--param-CPU-simuCycle") v "param-CPU-simuCycle"
    (set! param-CPU-simuCycle (string->number v))]

  [("--param-saved-params") v "param-saved-params"
    (set! param-saved-params v)]

  [("--param-saved-sizes") v "param-saved-sizes"
    (set! param-saved-sizes v)]
)




; STEP Saved params here
(cond
  [(equal? param-saved-params "spectre")
    (void)
  ]
  
  [(equal? param-saved-params "DoM")
    (set! param-cache-useHit #t)
    (set! param-enable-DoM #t)
  ]
  
  [(equal? param-saved-params "DoM_arbiterPriority")
    (set! param-cache-useHit #t)
    (set! param-enable-DoM #t)
    (set! param-enable-arbiterPriority #t)
  ]
  
  [(equal? param-saved-params "invisiSpec")
    (set! param-obsvType "cacheState")
    (set! param-cache-useHit #t)
    (set! param-enable-invisiSpec #t)
  ]
  
  [(equal? param-saved-params "invisiSpec_arbiterPriority")
    (set! param-obsvType "cacheState")
    (set! param-cache-useHit #t)
    (set! param-enable-invisiSpec #t)
    (set! param-enable-arbiterPriority #t)
  ]
  
  [(equal? param-saved-params "GhostMinion")
    (set! param-obsvType "cacheState")
    (set! param-cache-useHit #t)
    (set! param-enable-invisiSpec #t)
    (set! param-enable-arbiterPriority #t)
    (set! param-enable-GhostMinion #t)
  ]
)

(cond
  [(equal? param-saved-sizes "spectre")
    ; find attack ~10s
    (void)
  ]
  
  [(equal? param-saved-sizes "DoM")
    ; find attack ~10s
    (void)
  ]
  
  [(equal? param-saved-sizes "DoM_arbiterPriority")
    ; find attack ~1200s
    (set! param-ROB-size 8)
    (set! param-CPU-simuCycle 7)
  ]
  
  [(equal? param-saved-sizes "invisiSpec")
    ; find attack ~10s
    (void)
  ]
  
  [(equal? param-saved-sizes "invisiSpec_arbiterPriority")
    ; find attack ~200s
    (set! param-CPU-simuCycle 6)
  ]
  
  [(equal? param-saved-sizes "GhostMinion")
    ; find attack ~400s
    (set! param-ROB-size 4)
    (set! param-CPU-simuCycle 8)
  ]
)




; STEP: Computed parameters
(define param-rf-size-log (inexact->exact (log param-rf-size 2)))
(define param-memi-size-log (inexact->exact (log param-memi-size 2)))
(define param-memd-size-log (inexact->exact (log param-memd-size 2)))

(define param-ROB-size-log (inexact->exact (log param-ROB-size 2)))

(define param-Dcache-timFct-len param-memd-size-log)
(define param-FDelay-timFct-len param-memi-size-log)

(define param-CPU-simuCycle-bv
  (bv (sub1 param-CPU-simuCycle)
      (inexact->exact (ceiling (log param-CPU-simuCycle 2)))))

(define param-EXinst-size (* 2 (- param-CPU-simuCycle 2)))
(define param-EXinst-size-log
  (inexact->exact (ceiling (log param-EXinst-size 2))))

(define param-brID-len param-EXinst-size-log)

(define param-ISASimulator-simuCycle param-EXinst-size)
(define param-ISASimulator-simuCycle-bv
  (bv (sub1 param-ISASimulator-simuCycle) param-EXinst-size-log))




; STEP: Constrains on the parameters
(when (equal? param-obsvType "cacheState") (assert param-cache-useHit))

(when param-enable-DoM (assert param-cache-useHit))

(when param-enable-invisiSpec (assert param-cache-useHit)
                             (equal? param-obsvType "cacheState")
                             (assert (not param-enable-DoM)))
(when param-enable-GhostMinion (assert param-enable-invisiSpec)
                               (assert param-enable-arbiterPriority))

