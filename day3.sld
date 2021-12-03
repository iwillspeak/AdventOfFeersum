(define-library (advent day3)
  (import (scheme base)
    (scheme write)
    (advent data))
  (export main)
  (begin

    ;;; Add two sets of bit lists together
    ;;
    ;; Takes a two lists of bits `left` and `right` and returns a new list with
    ;; the bit-count for each summed. e.g. `(add-bits '(0 1) '(1 1))` -> `(1 2)`
    (define (add-bits left right)
      (define (add-bits-iter left right acc)
        (if (null? left)
          (reverse acc)
          (add-bits-iter (cdr left) (cdr right) (cons (+ (car left) (car right)) acc))))
      (add-bits-iter left right '()))

    ;;; Count the Bits in a list of bit-lists
    ;;
    ;; Given a list of lists of bits this function sums the number of bits set
    ;; at each position.
    (define (count-bits bits)
      (define (count-bits-iter bits acc)
        (if (null? bits)
          acc
          (let ((current (car bits))
                (bits (cdr bits)))
            (count-bits-iter bits (add-bits current acc)))))
      (count-bits-iter bits (make-list (length (car bits)) 0)))

    ;;; Find the most common bit pattern
    ;;
    ;; When given a list of bits this function finds the most common bit set in
    ;; each position (0 or 1). e.g. for `(most-common-bits '(1 0) '(1 1) '(0 0))`
    ;; it would return `(1 0)`.
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

    ;;; Bitwise inverse of a given bit-list.
    (define (inverse-bits bits)
      (if (null? bits)
        '()
        (cons (if (= 1 (car bits)) 0 1) (inverse-bits (cdr bits)))))

    ;;; Convert a bit list to a number.
    (define (bits-to-int bits)
      (define (bits-to-int-iter bits acc)
        (if (null? bits)
          acc
          (bits-to-int-iter
            (cdr bits)
            (+ (car bits) (* 2 acc)))))
      (bits-to-int-iter bits 0))

    ;;; Power Function
    ;;
    ;; Given a bit pattern from a diagnostic report calculate the current power
    ;; consumption. This extracts gamma and delta from it and multiplies for
    ;; the power.
    (define (power-from-bits bits)
      (let ((gamma (bits-to-int bits))
            (delta (bits-to-int (inverse-bits bits))))
        (* gamma delta)))

    ;;; Power from Diagnostic Report
    ;;
    ;; Takes the raw bits from a diagnostic report and returns the power use.
    (define (power-from-diag diag-data)
      (power-from-bits (most-common-bits diag-data)))

    ;;; Main entry point
    (define (main)
      (display "test: ")(display (power-from-diag day3-test))(newline)
      (display "real: ")(display (power-from-diag day3-real))(newline)
      )
))
