#!r6rs
(library (quickcheck)
(export quickcheck
        $integer
        $real
        $boolean
        $char
        $string
        $symbol
        $pair
        $list
        $vector
        $bytevector
        check
        implies
        ->
        one-of
        elements
        return
        $=>
        >>=
        (rename ($call call))
        such-that
        )
(import (rnrs)
        (srfi :27 random-bits)
        ;(srfi :48 intermediate-format-strings)
        (ice-9 format)
        )

(define (build-list size proc)
  (define (loop i l)
    (if (negative? i)
        l
        (loop (- i 1) (cons (proc i) l))))
  (loop (- size 1) '()))

(define ($integer)
  (random-integer 256))

(define $real random-real)

(define ($boolean)
  (zero? (random-integer 2)))

(define ($char)
  (integer->char ($integer)))

(define ($string)
  (list->string (($list $char))))

(define ($symbol)
  (string->symbol ($string)))

(define ($pair car cdr)
  (lambda ()
    (cons (car) (cdr))))

(define ($list generator)
  (lambda ()
    (build-list ($integer)
                (lambda (_) (generator)))))

(define ($vector generator)
  (lambda ()
    (list->vector (($list generator)))))

(define ($bytevector)
  (u8-list->bytevector (($list $integer))))

(define num-tests 100)
(define max-tests 1000)

(define (call proc) (proc))

(define-condition-type &predicate-failed &condition
  make-predicate-failed-condition
  predicate-failed-condition?)

(define (implies antecedent consequent)
  (if (antecedent)
      (consequent)
      (raise (make-predicate-failed-condition))))

(define-syntax ->
  (syntax-rules ()
    ((-> p q)
     (implies (lambda () p) (lambda () q)))))

(define (quickcheck test . generators)
  (define (fail i args)
    (format #t "Falsifiable after ~a tests: ~s~%" (+ 1 i) args))
  (define (succeed i)
    (format #t "OK: Passed ~a tests~%" i))
  (define (loop i j)
    (if (or (= i num-tests)
            (= j max-tests))
        (succeed i)
        (let* ((items (map call generators)))
          (guard (exn
                  ((predicate-failed-condition? exn)
                   (loop i (+ j 1)))
                  (else (fail i items)))
            (if (apply test items)
                (loop (+ i 1) (+ i j))
                (fail i items))))))
  (loop 0 0))

(define (vector-pick vector)
  (vector-ref vector (random-integer (vector-length vector))))

(define (elements list)
  (let ((v (list->vector list)))
    (lambda ()
      (vector-pick v))))

(define (return elem)
  (lambda () elem))

(define (one-of gen0 . genrest)
  ;; should this take generators, or just items?
  (lambda ()
    (call (call (elements (cons gen0 genrest))))))

;; I've been going back and forth over names for this, and I haven't
;; found one I like. Current candidates are: map (and just letting the
;; user rename themselves), fmap, convert, morph, generator-map, $map, $=>
(define ($=> g f)
  (lambda ()
    (f (g))))

(define (>>= m f) ;; call it bind?
  (lambda () ((f (m)))))

(define ($call f . args)
  (lambda ()
    ((apply f args))))

(define (such-that test? gen)
  (>>= gen
       (lambda (x)
         (if (test? x)
             (return x)
             (such-that test? gen)))))

)
