#lang rosette

(require
  "ISASimulator.rkt" "CPU/CPU.rkt"
  "sym-state/rf.rkt" "sym-state/memi.rkt" "sym-state/memd.rkt"
  "abs-module/brPred.rkt" "abs-module/absFifo2.rkt" "CPU/alu.rkt"
  "CPU/cache.rkt" "lib/lib.rkt"
)


(define (simu ISASimulator-cfg CPU-cfg param-debug-print-on)
  (define ISASimulator (init-ISASimulator
    ISASimulator-cfg param-ISASimulator-simuCycle
    param-ISASimulator-resetsym-cycle param-debug-print-on))
  (define CPU (init-CPU
    CPU-cfg
    param-CPU-simuCycle param-CPU-resetsym-cycle param-debug-print-on))

  (simu-ISASimulator! ISASimulator param-ISASimulator-simuCycle-bv)
  (simu-CPU! CPU param-CPU-simuCycle-bv)

  (list ISASimulator CPU)
)

(define ISASimulator-cfg (void))
(define CPU-cfg (void))


(define (veriCorr)

  (match-define (list ISASimulator-cfg-0 ISASimulator-cfg-1)
                (init-ISASimulator-cfg-pair))
  (match-define (list CPU-cfg-0 CPU-cfg-1)
                (init-CPU-cfg-pair ISASimulator-cfg-0 ISASimulator-cfg-1))

  (set! ISASimulator-cfg ISASimulator-cfg-0)
  (set! CPU-cfg CPU-cfg-0)

  (match-define (list ISASimulator CPU) (simu ISASimulator-cfg-0 CPU-cfg-0 #f))

  (printf (~a "Finish Generating SMT Formulas.\n"))



  (define sol (verify (assert-debug-commitLog
    (ISASimulator-debug-commitLog ISASimulator)
    (CPU-debug-commitLog CPU))))

  (when (sat? sol)
    (printf (~a "Find Counterexample.\n"))

    (set! ISASimulator-cfg-0 (ISASimulator-cfg-evaluate ISASimulator-cfg-0 sol))
    (set! ISASimulator-cfg-1 (ISASimulator-cfg-evaluate ISASimulator-cfg-1 sol))
    (set! CPU-cfg-0 (CPU-cfg-evaluate CPU-cfg-0 sol))
    (set! CPU-cfg-1 (CPU-cfg-evaluate CPU-cfg-1 sol))

    (match-define (list t0 t1) (simu ISASimulator-cfg-0 CPU-cfg-0 #t))
    (set! ISASimulator t0)
    (set! CPU t1)


    (printf (~a "Finish SMT Result Evaluation.\n"))

    (printf (~a
      "cfg: " ISASimulator-cfg-0 "\n"
      "ISASimulator: " ISASimulator "\n"
      "CPU: " CPU "\n"
    ))
  )

  (when (not (sat? sol)) (printf (~a "No Counterexample.\n")))
)


(define (testMe)
  (if param-debug-assert
    (begin
      (define veri-secu-test
        (test-suite+ "Tests for veriCorr"
                     (test-case+ "get-bug-info" (veriCorr))))
      (time (run-tests veri-secu-test))
      (when (sat? returnValue)
        (println returnValue)
        (printf (~a "cfg: " (ISASimulator-cfg-evaluate ISASimulator-cfg returnValue) "\n"))
        (simu (ISASimulator-cfg-evaluate ISASimulator-cfg returnValue) (CPU-cfg-evaluate CPU-cfg returnValue) #t)
      )
    )
    (time (veriCorr)))
)
(testMe)

