(define-library (advent day1)
    (import (scheme base)
      (scheme write)
      (scheme cxr)
      (feersum builtin macros)
      (prefix (advent data) data/))
    (export main)
    (begin
        ;;; Count Incrementing Depths
        ;;;
        ;;; Takes a list of `depths` and returns the number of times the depth increases.
        (define (count-increase depths)
          (define (count-increase-iter current remaining acc)
            (if (null? remaining)
                acc
                (let* ((next (car remaining))
                      (remaining (cdr remaining))
                      (diff (- next current)))
                  (count-increase-iter next remaining (if (> diff 0) (+ 1 acc) acc)))))
          (count-increase-iter (car depths) (cdr depths) 0))

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

        (define (sum3 list)
          (+ (car list) (cadr list) (caddr list)))

        (define (main)
          (display (count-increase data/day1-test))(newline)
          (display (count-increase data/day1-real))(newline)
          (display (count-increase (map1 sum3 (windows 3 data/day1-test))))(newline)
          (display (count-increase (map1 sum3 (windows 3 data/day1-real))))(newline))
        ))
