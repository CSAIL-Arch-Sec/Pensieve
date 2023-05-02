#lang rosette

(require
  "../lib/lib.rkt" "../sym-state/mem.rkt"
  "../abs-module/absArbiter.rkt" "../abs-module/absFifo.rkt"
  "../abs-module/absBufferGM.rkt" "../abs-module/absDelay.rkt"
)
(require "../sym-state/memd.rkt") ; for debug

(provide
  init-cache-cfg cache-cfg-evaluate
  init-cache cache-dataoutValid dataout-cache! checkHitReq-cache!
  checkDelayedReq-cache! datain-cache! cache-datainReady squash-cache!
  squashPartial-cache! cache-obsv tick-cache! cache-cfg? cache?
)


; PART cache-cfg
(struct cache-cfg (absArbiter-cfg isHit invisi-isHit absDelay-cfg
                   param-timFct-len param-simuCycle)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (cache-cfg->string this) port))]
)


(define (init-cache-cfg param-timFct-len param-fanin param-simuCycle
                        param-symType)
  (cache-cfg
    (init-absArbiter-cfg param-timFct-len param-fanin param-symType)

    ; TODO: move these two uninterpreted function to abs-module folder
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 1 1))]
      [(equal? param-symType "func_sym")
        (build-unfuncbv (* param-simuCycle (add1 param-timFct-len)) 1)])
    (cond
      [(equal? param-symType "func_concrete")
        (lambda (ignore) (bv 1 1))]
      [(equal? param-symType "func_sym")
        (build-unfuncbv (* 2 param-simuCycle (add1 param-timFct-len)) 1)])
    (cond
      [param-enable-GhostMinion
        (init-absBufferGM-cfg param-timFct-len param-simuCycle param-symType)]
      [else
        (init-absFifo-cfg param-timFct-len param-simuCycle param-symType)])
    param-timFct-len
    param-simuCycle)
)


(define (cache-cfg-evaluate cache-cfg-sym sol)
  (define absArbiter-cfg (cache-cfg-absArbiter-cfg cache-cfg-sym))
  (define isHit (cache-cfg-isHit cache-cfg-sym))
  (define invisi-isHit (cache-cfg-invisi-isHit cache-cfg-sym))
  (define absDelay-cfg (cache-cfg-absDelay-cfg cache-cfg-sym))
  (define param-timFct-len (cache-cfg-param-timFct-len cache-cfg-sym))
  (define param-simuCycle (cache-cfg-param-simuCycle cache-cfg-sym))

  (cache-cfg
    (absArbiter-cfg-evaluate absArbiter-cfg sol)
    (evaluate isHit sol)
    (evaluate invisi-isHit sol)
    (absDelay-cfg-evaluate absDelay-cfg sol)
    param-timFct-len
    param-simuCycle
  )
)


(define (cache-cfg->string cache-cfg)
  "cache-cfg"
)


; PART cache
(struct cache (absArbiter history-timFct history-valid history-return isHit
               invisi-timFct invisi-brID invisi-valid invisi-return invisi-isHit
               hitValid hitEntry delayValid delayReqId absDelay mem encoder1
               decoder1 encoder2 decoder2 clk)
  #:mutable #:transparent
  #:methods gen:custom-write
    [(define (write-proc this port mode)
             (write-string (cache->string this) port))]
)


; TODO: param-addr-len can be removed from the arguments
(define (init-cache cache-cfg mem param-addr-len param-data-len param-reqId-len)
  (define absArbiter-cfg (cache-cfg-absArbiter-cfg cache-cfg))
  (define isHit (cache-cfg-isHit cache-cfg))
  (define invisi-isHit (cache-cfg-invisi-isHit cache-cfg))
  (define absDelay-cfg (cache-cfg-absDelay-cfg cache-cfg))
  (define param-timFct-len (cache-cfg-param-timFct-len cache-cfg))
  (define param-simuCycle (cache-cfg-param-simuCycle cache-cfg))
  (define param-simuCycle-log
    (inexact->exact (ceiling (log param-simuCycle 2))))

  (define param-entry1-len (+ 1 param-reqId-len param-data-len))
  (define param-entry2-len (+ 1 param-reqId-len param-data-len))
  (define absDelay (init-absDelay absDelay-cfg param-entry2-len))
  (define absArbiter (init-absArbiter absArbiter-cfg absDelay param-entry1-len))

  (define (encoder1 underSpec reqId addr data rdwt mem)
    (when param-debug-assert (bug-assert 
      rdwt
      #:msg "cache does not support write for now"))
    (concat (bool->bitvector underSpec) reqId (mem-ref mem addr))
  )
  (define (decoder1 entry)
    (define underSpec (extract (- param-entry1-len 1) (- param-entry1-len 1)
                               entry))
    (define reqId (extract (- param-entry1-len 2) param-data-len entry))
    (define data (extract (- param-data-len 1) 0 entry))
    (list (bitvector->bool underSpec) reqId data)
  )
  
  (define (encoder2 underSpec reqId data)
    (concat (bool->bitvector underSpec) reqId data)
  )
  (define (decoder2 entry)
    (define underSpec (extract (- param-entry2-len 1) (- param-entry2-len 1)
                               entry))
    (define reqId (extract (- param-entry2-len 2) param-data-len entry))
    (define data (extract (- param-data-len 1) 0 entry))
    (list (bitvector->bool underSpec) reqId data)
  )
  
  (cache
    absArbiter

    (initZeroVec-array param-simuCycle param-timFct-len)
    (initZeroVec-array param-simuCycle 1)
    (initZeroVec-array param-simuCycle 1)
    isHit
    (initZeroVec-array param-simuCycle param-timFct-len)
    (initZeroVec-array param-simuCycle param-brID-len)
    (initZeroVec-array param-simuCycle 1)
    (initZeroVec-array param-simuCycle 1)
    invisi-isHit
    #f
    (bv 0 param-entry2-len)

    #f
    (bv 0 param-reqId-len)

    absDelay

    mem
    encoder1 decoder1 encoder2 decoder2
    (bv 0 param-simuCycle-log))
)


(define (cache-dataoutValid cache)
  (define delayValid (cache-delayValid cache))
  (define hitValid (cache-hitValid cache))
  (define absDelay (cache-absDelay cache))

  (absDelay-dataoutValid absDelay)
)


(define (dataout-cache! cache)
  (define history-return (cache-history-return cache))
  (define invisi-return (cache-invisi-return cache))
  (define absDelay (cache-absDelay cache))
  (define decoder2 (cache-decoder2 cache))
  (define clk (cache-clk cache))

  (match-define (list underSpec reqId data)
                (decoder2 (dataout-absDelay! absDelay)))

  ; TODO: not only put return into observation,
  ;       but also put into input to hit uninterFunc
  (when (and param-cache-useHit (not param-enable-invisiSpec))
    (set-array! history-return clk (bv 1 1)))
  (when (and param-cache-useHit param-enable-invisiSpec)
    (if underSpec
      (set-array! invisi-return clk (bv 1 1))
      (set-array! history-return clk (bv 1 1))))

  (list reqId data)
)


; NOTE: we assume the hit return and delay return
;       do not contend with miss return
(define (checkHitReq-cache! cache)
  (define hitValid (cache-hitValid cache))
  (define hitEntry (cache-hitEntry cache))
  (define decoder2 (cache-decoder2 cache))

  (set-cache-hitValid! cache #f)
  (match-define (list underSpec reqId data) (decoder2 hitEntry))
  (list (and param-cache-useHit hitValid) reqId data)
)


(define (checkDelayedReq-cache! cache)
  (define delayValid (cache-delayValid cache))
  (define delayReqId (cache-delayReqId cache))

  (set-cache-delayValid! cache #f)
  (list delayValid delayReqId)
)


(define (datain-cache! cache underSpec brID reqId addr data rdwt fanId timFct)
  (define absArbiter (cache-absArbiter cache))
  (define mem (cache-mem cache))
  (define encoder1 (cache-encoder1 cache))

  (when param-cache-useHit
    (when param-debug-assert (bug-assert
      (not (cache-hitValid cache))
      #:msg "datain-cache!: cache hit should be returned no matter what")))

  (datain-absArbiter! absArbiter (encoder1 underSpec reqId addr data rdwt mem)
                      brID fanId timFct)
)


(define (cache-datainReady cache)
  (define absArbiter (cache-absArbiter cache))

  (absArbiter-datainReady absArbiter)
)


; TODO: merge invisi-timFct to history-timFct


(define (squash-cache! cache)
  (define absArbiter (cache-absArbiter cache))
  (define absDelay (cache-absDelay cache))

  (squash-absArbiter! absArbiter)
  (drainSquash-absDelay! absDelay)
)


(define (squashPartial-cache! cache misPredBr-brID)
  (define absArbiter (cache-absArbiter cache))
  (define absDelay (cache-absDelay cache))

  (squashPartial-absArbiter! absArbiter misPredBr-brID)
  (drainSquashPartial-absDelay! absDelay misPredBr-brID)
)


(define (cache-obsv cache)
  (cond
    [(equal? param-obsvType "memTrace")
      (define absDelay (cache-absDelay cache))
      (absDelay-obsv absDelay)]

    [(equal? param-obsvType "cacheState")
      (define history-timFct (cache-history-timFct cache))
      (define history-valid (cache-history-valid cache))
      (define history-return (cache-history-return cache))
      (concat (array->bv history-timFct) (array->bv history-valid)
              (array->bv history-return))]

    [(equal? param-obsvType "commitPC")
      (when param-debug-assert
        (bug-assert #f #:msg "cache-obsv: param-obsvType"))]
  )
)


(define (cache->string cache)
  (define history-timFct (cache-history-timFct cache))
  (define history-valid (cache-history-valid cache))
  (define invisi-timFct (cache-invisi-timFct cache))
  (define invisi-valid (cache-invisi-valid cache))
  (define absArbiter (cache-absArbiter cache))
  (define absDelay (cache-absDelay cache))
  
  ; (~a absArbiter "\n" absDelay)
  (~a "\n"
    "  history: " history-valid history-timFct
    ", invisi: " invisi-valid invisi-timFct "\n"
    "  absArbiter: " absArbiter "\n"
    "  absDelay:   " absDelay
  )
)


(define (tick-cache! cache)
  (define absArbiter (cache-absArbiter cache))
  (define history-timFct (cache-history-timFct cache))
  (define history-valid (cache-history-valid cache))
  (define isHit (cache-isHit cache))
  (define invisi-timFct (cache-invisi-timFct cache))
  (define invisi-brID (cache-invisi-brID cache))
  (define invisi-valid (cache-invisi-valid cache))
  (define invisi-isHit (cache-invisi-isHit cache))
  (define absDelay (cache-absDelay cache))
  (define mem (cache-mem cache))
  (define decoder1 (cache-decoder1 cache))
  (define encoder2 (cache-encoder2 cache))
  (define clk (cache-clk cache))

  (when (and param-debug-assert param-enable-DoM) (bug-assert
    (not (cache-delayValid cache))
    #:msg "tick-cache!: cache delay should be returned no matter what"))
  (when (and param-debug-assert param-cache-useHit) (bug-assert
    (not (cache-hitValid cache))
    #:msg "tick-cache!: cache hit should be returned no matter what"))

  (when (absArbiter-dataoutValid absArbiter)
    (when param-debug-assert (bug-assert
      (absDelay-datainReady absDelay)
      #:msg "tick-cache!: absDelay-datainReady is False"))

    ; PART: get one request from the arbiter
    (match-define (list entry1 brID timFct) (dataout-absArbiter! absArbiter))
    (match-define (list underSpec reqId data) (decoder1 entry1))
    (define entry2 (encoder2 underSpec reqId data))

    (cond
      [(and param-cache-useHit (not param-enable-invisiSpec))
        ; PART: update the cache hit history
        (set-array! history-timFct clk timFct)
        (set-array! history-valid clk (bv 1 1))
        (define hit (bitvector->bool (isHit
          (concat (array->bv history-timFct) (array->bv history-valid)))))

        ; PART: DoM
        ; TODO: maybe remove the set-then-unset can improve the performance
        (when (and param-enable-DoM underSpec)
            (resetToZero-array! history-timFct clk)
            (resetToZero-array! history-valid clk))

        ; PART: cache hit
        (when hit
          (set-cache-hitValid! cache #t)
          (set-cache-hitEntry! cache entry2))

        ; PART: cache miss
        (when (not hit)
          ; PART: DoM
          (when (and param-enable-DoM underSpec)
            (set-cache-delayValid! cache #t)
            (set-cache-delayReqId! cache reqId))
          (when (not (and param-enable-DoM underSpec))
            (datain-absDelay! absDelay entry2 brID timFct)))
      ]

      [(and param-cache-useHit param-enable-invisiSpec)
        ; PART: update the cache hit history
        (define hit #t)
        (if underSpec
          (begin
            (set-array! invisi-timFct clk timFct)
            (when param-enable-GhostMinion (set-array! invisi-brID clk brID))
            (set-array! invisi-valid clk (bv 1 1))

            (define (maskThisEntry i-bv)
              (define i-brID (array-ref invisi-brID i-bv))
              (bvugt i-brID brID))
            (define invisi-timFct-masked (if param-enable-GhostMinion
              (array->masked invisi-timFct maskThisEntry)
              invisi-timFct))
            (define invisi-valid-masked (if param-enable-GhostMinion
              (array->masked invisi-valid maskThisEntry)
              invisi-valid))
            (set! hit (bitvector->bool (invisi-isHit
              (concat (array->bv history-timFct) (array->bv history-valid)
                      (array->bv invisi-timFct-masked)
                      (array->bv invisi-valid-masked))))))
          (begin
            (set-array! history-timFct clk timFct)
            (set-array! history-valid clk (bv 1 1))
            (set! hit (bitvector->bool (isHit
              (concat (array->bv history-timFct) (array->bv history-valid)))))))
          
        ; PART: cache hit
        (when hit
          (set-cache-hitValid! cache #t)
          (set-cache-hitEntry! cache entry2))
      
        ; PART: cache miss
        (when (not hit)
          (datain-absDelay! absDelay entry2 brID timFct))
      ]

      [else
        (datain-absDelay! absDelay entry2 brID timFct)
      ]
    )
  )
  
  (tick-absArbiter! absArbiter)
  (tick-absDelay! absDelay)
  (tick-mem! mem)
  (set-cache-clk! cache (bvadd1 clk))
)


(define (testMe)

  (match-define (list memd-cfg-1 memd-cfg-2)
                (init-memd-cfg-pair param-memd-size param-reg-len
                                    "vec_concrete"))
  (define memd (init-memd memd-cfg-2 10))
  (define cache-cfg (init-cache-cfg 2 8 10 "func_concrete"))
  (define cache (init-cache cache-cfg memd 2 4 3))
  (printf (~a cache "\n"))

  (printf (~a (cache-dataoutValid cache) "\n"))
  ;(printf (~a (dataout-cache! cache) "\n"))
  (printf (~a (datain-cache! cache #f (bv 1 param-brID-len) (bv 2 3) (bv 0 2)
                             (bv 15 4) #t (bv 0 3) (bv 2 2))
              "\n"))
  (printf (~a (cache-datainReady cache) "\n"))
  (printf (~a cache "\n"))

  (tick-cache! cache)
  (printf (~a "-----------------\n"))

  (printf (~a (cache-dataoutValid cache) "\n"))
  (printf (~a (dataout-cache! cache) "\n"))
  (printf (~a (datain-cache! cache #f (bv 3 param-brID-len) (bv 2 3) (bv 3 2)
                             (bv 15 4) #t (bv 1 3) (bv 2 2))
              "\n"))
  (printf (~a (cache-datainReady cache) "\n"))
  (printf (~a cache "\n"))
)
;(testMe)

