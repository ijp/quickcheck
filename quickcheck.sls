#!r6rs
(library (quickcheck quickcheck)
(export quickcheck
        $integer
        )
(import (rnrs)
        (srfi :27 random-bits)
        ;(srfi :48 intermediate-format-strings)
        (ice-9 format)
        )

(define ($integer)
  (random-integer 256))

(define num-tests 100)

(define (call proc) (proc))

(define (quickcheck test . generators)
  (define (fail i args)
    (format #t "Falsifiable after ~a tests: ~s~%" (+ 1 i) args))
  (define (succeed i)
    (format #t "OK: Passed ~a tests~%" i))
  (define (loop i)
    (if (= i num-tests)
        (succeed i)
        (let* ((items (map call generators))
               (result (guard (exn
                               (else #f))
                         (apply test items))))
          (if result
              (loop (+ i 1))
              (fail i items)))))
  (loop 0))


)
