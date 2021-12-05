(define-library (advent day4)
  (import (scheme base)
    (scheme write)
    (feersum builtin macros)
    (advent data)
    (advent utils))
  (export main)
  (begin

    ;;; Make a Board Vector
    ;;
    ;; Boards are random-access so we store them in vectors not lists.
    (define (make-board contents)
      (define (fill-board board i contents)
        (when (pair? contents)
          (vector-set! board i (cons (car contents) #f))
          (fill-board board (+ 1 i) (cdr contents))))
      (define board (make-vector 25))
      (fill-board board 0 contents)
      board)

    ;;; Get an element from a board
    (define (board-get board i j)
      (vector-ref board (+ (* 5 i) j)))

    ;;; Get the number from a board element
    (define (board-get-num board i j)
      (car (board-get board i j)))

    ;;; Get the number from a board element
    (define (board-get-mark board i j)
      (cdr (board-get board i j)))

    ;;; Set the mark on a given board element
    (define (board-set-mark! board i j)
      (set-cdr! (board-get board i j) #t))

    ;;; Check the board to see if it has won
    (define (check-board board)
      (define (check-row board row)
        (and
          (board-get-mark board 0 row)
          (board-get-mark board 1 row)
          (board-get-mark board 2 row)
          (board-get-mark board 3 row)
          (board-get-mark board 4 row)))
      (define (check-col board col)
        (and
          (board-get-mark board col 0)
          (board-get-mark board col 1)
          (board-get-mark board col 2)
          (board-get-mark board col 3)
          (board-get-mark board col 4)))
      (or
        (check-row board 0)
        (check-row board 1)
        (check-row board 2)
        (check-row board 3)
        (check-row board 4)
        (check-col board 0)
        (check-col board 1)
        (check-col board 2)
        (check-col board 3)
        (check-col board 4)))

    ;;; Print out a board
    (define (display-board board)
      (define (display-one board i j)
        (define num (board-get-num board i j))
        (if (< num 10) (display " "))
        (if (board-get-mark board i j)
          (begin (display ".")(display num))
          (begin (display " ")(display num))))
      (define (display-row board row)
        (display-one board 0 row)(display " ")
        (display-one board 1 row)(display " ")
        (display-one board 2 row)(display " ")
        (display-one board 3 row)(display " ")
        (display-one board 4 row)(newline))
      (display "\n")
      (display-row board 0)
      (display-row board 1)
      (display-row board 2)
      (display-row board 3)
      (display-row board 4))

    ;;; Make boards
    (define (make-boards boards)
      (if (pair? boards)
        (cons (make-board (car boards))
          (make-boards (cdr boards)))
        '()))

    ;;; Main entry point
    (define (main)
      (define boards (make-boards (cdr day4-test)))
      (display "test: \n")(map1 display-board boards)(newline)
      )
))
