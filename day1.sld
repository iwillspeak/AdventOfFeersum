(define-library (advent day1)
    (import (scheme base)
      (scheme write)
      (scheme cxr)
      (advent utils)
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

        (define (sum3 list)
          (+ (car list) (cadr list) (caddr list)))

        (define (main)
          (display (count-increase data/day1-test))(newline)
          (display (count-increase data/day1-real))(newline)
          (display (count-increase (map1 sum3 (windows 3 data/day1-test))))(newline)
          (display (count-increase (map1 sum3 (windows 3 data/day1-real))))(newline))
        ))
