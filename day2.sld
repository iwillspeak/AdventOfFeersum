(define-library (advent day2)
    (import (scheme base)
      (scheme write)
      (scheme cxr)
      (feersum builtin macros)
      (prefix (advent data) data/))
    (export main)
    (begin

      ;;; Find the Distance Traveled
      ;;
      ;; Calculates the final position and then works out the distance from the
      ;; origin.
      (define (find-distance moves)
        ;;; Apply the moves to the given position.
        (define (apply-moves pos moves)
          (if (null? moves)
            pos
            (let ((new-pos 
              (cond ((eq? (caar moves) 'forward) (cons (+ (car pos) (cadar moves)) (cdr pos)))
                ((eq? (caar moves) 'up) (cons (car pos) (- (cdr pos) (cadar moves))))
                ((eq? (caar moves) 'down) (cons (car pos) (+ (cdr pos) (cadar moves)))))))
              (apply-moves new-pos (cdr moves)))))
        (define final-pos (apply-moves (cons 0 0) moves))
        (display final-pos)(newline)
        (* (car final-pos) (cdr final-pos)))


      (define (main)
        (display (find-distance data/day2-test))(newline)
        (display (find-distance data/day2-real))(newline))
        ))
