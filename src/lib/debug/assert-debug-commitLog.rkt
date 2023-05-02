#lang rosette

(provide assert-debug-commitLog)


(define (assert-debug-commitLog log-ISASimulator log-CPU)
  (define len-CPU (length-bv log-CPU (bitvector 32)))

  (set! log-ISASimulator (take-bv log-ISASimulator len-CPU))

  (define (assert-entry entry-0 entry-1)
    (if (equal? (bv 3 32) (length-bv entry-0 (bitvector 32)))
      (begin
        (assert (equal? (bv 3 32) (length-bv entry-1 (bitvector 32))))
        (assert (bveq (first entry-0) (first entry-1)))
        (assert (bveq (second entry-0) (second entry-1)))
        (assert (bveq (third entry-0) (third entry-1))))
      (begin
        (assert (equal? (bv 1 32) (length-bv entry-1 (bitvector 32))))
        (assert (bveq (first entry-0) (first entry-1))))))

  (for-each assert-entry log-ISASimulator log-CPU)
)

