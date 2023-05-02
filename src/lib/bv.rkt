#lang rosette

(provide build-unfuncbv build-symbv)


(define (build-unfuncbv from to)
  (define-symbolic* unfuncbv (~> (bitvector from) (bitvector to)))
  unfuncbv
)


(define (build-symbv size)
  (define-symbolic* symbv (bitvector size))
  symbv
)

