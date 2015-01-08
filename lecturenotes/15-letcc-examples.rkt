#lang racket

; Here we simulate a very simple kind of exception handling mechanism
; with first-class continuations.
; try evaluating "(h)" in the read-eval-print-loop

(define exceptionhandler (lambda (msg) (display "unhandled exception")))

(define (f n)
  (+ 5
     (if (zero? n) (exceptionhandler "division by zero")
         (/ 8 n))))

(define (g n)
  (+ (f n) (f n)))

(define (h)
  (let/cc k
    (begin
      (set! exceptionhandler (lambda (msg) (begin
                                             (displayln msg)
                                             (k))))
      (displayln (g 1))
      (displayln (g 0))
      (displayln (g 2)))))
      


; Here we encode a simple debugger with support for breakpoints


; the breakpoint variable stores the continuation at the current breakpoint
(define breakpoint false) ; initalized with a dummy value

; the repl variable stores the continuation that jumps to the read-eval-print loop
(define repl false)       ; initialized with a dummy value

; the break function captures the current continuation, stores it, and jumps to the REPL
(define (break) (let/cc k
                  (begin
                    (set! breakpoint k)
                    (repl))))

; to continue, we jump to the previously stored continuation
(define (continue)
  (breakpoint))

; a simple test program of our "debugger"

(define (main)
  (display "1")
  (break)
  (display "2")
  (break)
  (display "3"))

; nothing to do after this, hence k is the REPL continuation
(let/cc k 
  (set! repl k))

