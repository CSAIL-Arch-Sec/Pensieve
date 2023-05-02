#lang rosette

(require "buginfo.rkt" "unittest.rkt" "assert-debug-commitLog.rkt")
(provide (all-from-out "buginfo.rkt") (all-from-out "unittest.rkt")
         (all-from-out "assert-debug-commitLog.rkt"))


(define (testMe)
  (define (get-bug)
    (define-symbolic a boolean?)
    (bug-assert a #:msg "hello world"))
  (define (get-bug2)
    (define-symbolic a boolean?)
    (bug-assert a #:msg (lambda (sol) (format "a is ~v" (evaluate a sol)))))

  (define (get-codebug)
    (define-symbolic a boolean?)
    (bug-assert a #:msg "hello world")
    (assume a)
    (assert a))

  (define (get-countercase)
    (define-symbolic a boolean?)
    (assert a))

  (define (get-countercase-bug)
    (define-symbolic a boolean?)
    (assert a)
    (bug-assert a #:msg "hello world"))

  (define (get-normal)
    (define-symbolic a boolean?)
    (assume a)
    (bug-assert a #:msg "hello world")
    (assert a))
  

  (define bug-info-tests
    (test-suite+
     "Tests for bug info"
     (test-case+ "get-bug-info" (get-bug))
     (test-case+ "get-bug2-info" (get-bug2))
     (test-case+ "get-codebug-info" (get-codebug))
     (test-case+ "get-countercase-info" (get-countercase))
     (test-case+ "get-countercase-bug-info" (get-countercase-bug))
     (test-case+ "get-normal-info" (get-normal))))

  (time (run-tests bug-info-tests))
)

;(testMe)

