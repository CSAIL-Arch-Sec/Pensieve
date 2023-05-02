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


(define (veriSpec)

  ; STEP Initialize Symbolic Values
  (match-define (list ISASimulator-cfg-0 ISASimulator-cfg-1)
                (init-ISASimulator-cfg-pair))
  (match-define (list CPU-cfg-0 CPU-cfg-1)
                (init-CPU-cfg-pair ISASimulator-cfg-0 ISASimulator-cfg-1))


  ; STEP Simu the 4 copies
  (match-define (list ISASimulator-0 CPU-0)
                (simu ISASimulator-cfg-0 CPU-cfg-0 #f))
  (match-define (list ISASimulator-1 CPU-1)
                (simu ISASimulator-cfg-1 CPU-cfg-1 #f))
  (printf (~a "Finish Symbolic Execution.\n"))


  ; STEP Assume
  (assume-ISASimulator-memTrace (ISASimulator-memTrace ISASimulator-0)
                                (ISASimulator-memTrace ISASimulator-1))


  ; STEP Assert
  (define sol (verify (assert-CPU-obsv (CPU-obsv CPU-0) (CPU-obsv CPU-1))))
  (printf (~a "Finish STM Solver.\n"))


  ; STEP Output Counter Example
  (when (sat? sol)
    (printf (~a "Find Counterexample.\n"))

    (set! ISASimulator-cfg-0 (ISASimulator-cfg-evaluate ISASimulator-cfg-0 sol))
    (set! ISASimulator-cfg-1 (ISASimulator-cfg-evaluate ISASimulator-cfg-1 sol))
    (set! CPU-cfg-0 (CPU-cfg-evaluate CPU-cfg-0 sol))
    (set! CPU-cfg-1 (CPU-cfg-evaluate CPU-cfg-1 sol))

    (match-define (list t0 t1) (simu ISASimulator-cfg-0 CPU-cfg-0 #t))
    (match-define (list t2 t3) (simu ISASimulator-cfg-1 CPU-cfg-1 #t))
    (set! ISASimulator-0 t0)
    (set! CPU-0 t1)
    (set! ISASimulator-1 t2)
    (set! CPU-1 t3)


    (printf (~a "Finish SMT Result Evaluation.\n"))

    (printf (~a
      "cfg: " (ISASimulator-cfg-pair->string
                ISASimulator-cfg-0 ISASimulator-cfg-1) "\n"
      "ISASimulator-0: " ISASimulator-0 "\n"
      "ISASimulator-1: " ISASimulator-1 "\n"
      "CPU-0: " CPU-0 "\n"
      "CPU-1: " CPU-1 "\n"
    ))
  )

  (when (not (sat? sol)) (printf (~a "No Counterexample.\n")))
)


(define (testMe)
  (if param-debug-assert
    (begin
      (define veri-secu-test
        (test-suite+ "Tests for veriSpec"
                     (test-case+ "get-bug-info" (veriSpec))))
      (time (run-tests veri-secu-test)))
    (time (veriSpec)))
)
(testMe)

