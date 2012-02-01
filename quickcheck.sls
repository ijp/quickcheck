#!r6rs
(library (quickcheck quickcheck)
(export quickcheck
        $integer
        $boolean
        $char
        $string
        $list
        $vector
        $bytevector
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

(define ($boolean)
  (zero? (random-integer 2)))

(define ($char)
  (integer->char ($integer)))

(define ($string)
  (list->string (($list $char))))

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
