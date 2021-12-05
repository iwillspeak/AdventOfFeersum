(define-library (advent utils)
    (import (scheme base)
      (feersum builtin macros))
    (export split-at windows map1)
    (begin

        ;;; Split a List at the given Pivot
        (define (split-at pivot list)
          (define (split-at-iter pos left list)
            (if (or (null? list) (= 0 pos))
              (cons (reverse left) list)
              (split-at-iter (- pos 1)
                (cons (car list) left)
                (cdr list))))
          (split-at-iter pivot '() list))

        ;;; Get the overlapping windows of `size` in `list`.
        (define (windows size list)
          (let* ((split (split-at size list))
                 (window (car split)))
            (if (< (length window) size)
              '()
              (cons window (windows size (cdr list))))))

        ;;; Map a function over a list
        (define (map1 f list)
          (if (null? list)
            '()
            (cons (f (car list)) (map1 f (cdr list)))))

        ))
