#lang rosette

(provide
  init-array initZeroVec-array initFalseVec-array initFromCopy-array
  array-ref set-array! array->bv array-equal resetToZero-array!
  resetToZeroVec-array! resetToFalseVec-array! resetToTrueVec-array!
  resetsym-bvVec-array! resetsym-boolVec-array! array-evaluate
  array->masked array->custom-string
)


; TODO save the param-len, element-evaluate
(struct array (content param-size param-len)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (array->string this) port))])

(define (vector-copy-lifted vector) 
  (for/all ([v vector #:exhaustive]) (vector-copy v))
)

(define (init-array content param-size param-len)
  (array
    (if (vector? content) (vector-copy-lifted content) content)
    param-size
    param-len
  )
)


(define (initZeroVec-array param-size param-len)
  (array
    (make-vector param-size (bv 0 param-len))
    param-size
    param-len
  )
)


(define (initFalseVec-array param-size)
  (array
    (make-vector param-size #f)
    param-size
    1
  )
)


(define (initFromCopy-array copyFrom)
  (init-array
    (array-content copyFrom)
    (array-param-size copyFrom)
    (array-param-len copyFrom))
)


(define (array-ref array pos)
  (if (vector? (array-content array))
      (vector-ref-bv (array-content array) pos)
      ((array-content array) pos)
  )
)


(define (set-array! array pos v)
  (if (vector? (array-content array))
      (vector-set!-bv (array-content array) pos v)
      (begin
        (define temp (array-content array))
        (set-array-content! array (lambda (x) (if (equal? pos x) v (temp x)))))
  )
)


(define (array->bv array)
  (define param-size (array-param-size array))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))
  
  (apply concat
         (build-list param-size
                     (lambda (i) (array-ref array (bv (- param-size (+ i 1))
                                                      param-size-log)))))
)


(define (array-equal array-1 array-2)
  (define param-size (array-param-size array-1))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))

  (define is-equal #t)
  (for ([i (in-range param-size)])
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    (set! is-equal (and is-equal (equal? (array-ref array-1 i-bv)
                                         (array-ref array-2 i-bv))))
  )
  is-equal
)


(define (resetToZero-array! array pos)
  (define param-len (array-param-len array))
  
  (set-array! array pos (bv 0 param-len))
)


(define (resetToZeroVec-array! array)
  (define param-size (array-param-size array))
  (define param-len (array-param-len array))
  
  (set-array-content! array (make-vector param-size (bv 0 param-len)))
)


(define (resetToFalseVec-array! array)
  (define param-size (array-param-size array))
  
  (set-array-content! array (make-vector param-size #f))
)


(define (resetToTrueVec-array! array)
  (define param-size (array-param-size array))
  
  (set-array-content! array (make-vector param-size #t))
)


(define (resetsym-bvVec-array! array)
  (define param-size (array-param-size array))
  (define param-size-log  (inexact->exact (ceiling (log param-size 2))))
  (define param-len (array-param-len array))
  
  (for ([i (in-range param-size)])
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    
    (when (term? (array-ref array i-bv))
      (define-symbolic* entry-new (bitvector param-len))
      (assume (bveq entry-new (array-ref array i-bv)))
      (set-array! array i-bv entry-new)))
)


(define (resetsym-boolVec-array! array)
  (define param-size (array-param-size array))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))
  
  (for ([i (in-range param-size)])
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    
    (when (term? (array-ref array i-bv))
      (define-symbolic* entry-new boolean?)
      (assume (equal? entry-new (array-ref array i-bv)))
      (set-array! array i-bv entry-new)))
)


(define (array->masked array maskThisEntry)
  (define param-size (array-param-size array))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))

  (define array-masked (initFromCopy-array array))
  (for ([i (in-range param-size)])
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    (when (maskThisEntry i-bv)
      (resetToZero-array! array-masked i-bv)))
  array-masked
)


(define (array->custom-string array indexEntry->string separator)
  (define param-size (array-param-size array))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))

  (define string (~a "("))
  (for ([i (in-range param-size)])
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    (define entry (array-ref array i-bv))
    (set! string (~a string (indexEntry->string i-bv entry) separator))
  )
  (~a string ")")
)


(define (array->string array)
  (array->custom-string array (lambda (i e)
    (if (bv? e) (bitvector->natural e) e)) " ")
)


(define (array-evaluate array-sym element-evaluate sol)
  (define param-size (array-param-size array-sym))
  (define param-size-log (inexact->exact (ceiling (log param-size 2))))
  (define param-len (array-param-len array-sym))

  (define (index-evaluate i)
    (define i-bv (integer->bitvector i (bitvector param-size-log)))
    (element-evaluate (array-ref array-sym i-bv) sol)
  )
  (array (build-vector param-size index-evaluate) param-size param-len)
)

