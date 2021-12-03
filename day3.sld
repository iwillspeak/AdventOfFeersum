(define-library (advent day3)
  (import (scheme base)
    (scheme write)
    (advent data))
  (export main)
  (begin

    ;; Add two sets of bit lists together
    (define (add-bits left right)
      (define (add-bits-iter left right acc)
        (if (null? left)
          (reverse acc)
          (add-bits-iter (cdr left) (cdr right) (cons (+ (car left) (car right)) acc))))
      (add-bits-iter left right '()))

    (define (count-bits bits)
      (define (count-bits-iter bits acc)
        (if (null? bits)
          acc
          (let ((current (car bits))
                (bits (cdr bits)))
            (count-bits-iter bits (add-bits current acc)))))
      (count-bits-iter bits '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

    (define (most-common-bits bits)
      (let* ((bits-len (length bits))
            (cutoff  (/ bits-len 2)))
        (define (most-common-bits-iter bits)
          (if (null? bits)
            '()
            (cons 
              (if (> (car bits) cutoff) 1 0)
              (most-common-bits-iter (cdr bits)))))
        (most-common-bits-iter (count-bits bits))))

    (define (inverse-bits bits)
      (if (null? bits)
        '()
        (cons (if (= 1 (car bits)) 0 1) (inverse-bits (cdr bits)))))

    (define (bits-to-int bits)
      (define (bits-to-int-iter bits acc)
        (if (null? bits)
          acc
          (bits-to-int-iter
            (cdr bits)
            (+ (car bits) (* 2 acc)))))
      (bits-to-int-iter bits 0))

    (define (power-from-bits bits)
      (let ((gamma (bits-to-int bits))
            (delta (bits-to-int (inverse-bits bits))))
        (* gamma delta)))

    (define (power-from-diag diag-data)
      (power-from-bits (most-common-bits diag-data)))

    (define (main)
      (display "test: ")(display (power-from-diag day3-test))(newline)
      (display "real: ")(display (power-from-diag day3-real))(newline)
      )
))
