(import (scheme write)
        (scheme base)
        (prefix (advent data) data/))

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

(display (count-increase data/day1-test))(newline)
(display (count-increase data/day1-real))(newline)
