(import (c211 image))
(import (c211 matrix))
(import (c211 tree))



;transpose procedure: takes a matrix mat and returns its transpose. The
; transpose of a matrix mat is another matrix t-mat whose number of rows is
; the number of columns of mat, and whose number of columns is the number of
; rows of mat, and the element at position row col of t-mat is the element
; at position col row of mat.

(define transpose-alt-version
  (lambda (mat)
    (matrix-generator (matrix-cols mat) (matrix-rows mat)
      (lambda (r c) (matrix-ref mat c r)))))

(define mat1 (vov->matrix '#(#(1 2 3 4 5) #(6 7 8 9 10))))

(define mat2
  (vov->matrix
    '#(#(1 2 3 4 5) #(-1 0 8 9 7) #(-6 2 1 7 4) #(10 3 -3 -6 0)
        #(7 6 4 2 5) #(8 1 2 2 3))))

(define transpose
  (lambda (mat)
    (let ([t-mat (make-matrix (matrix-cols mat) (matrix-rows mat))]
          [row-end (matrix-rows mat)]
          [col-end (matrix-cols mat)])
      (let loop-r ([row 0])
        (if (< row row-end)
            (begin
              (let loop-c ([col 0])
                (if (< col col-end)
                    (begin
                      (matrix-set! t-mat col row (matrix-ref mat row col))
                      (loop-c (add1 col)))))
              (loop-r (add1 row)))))
      t-mat)))




; Magic - Square

;A) sum-rows procedure: takes a matrix mat and returns a list of the sums of
; each row in the matrix (starting from the first row).

(define mat3
  (vov->matrix
    '#(#(4 14 15 1) #(9 7 6 12) #(5 11 10 8) #(16 2 3 13))))

(define sum-rows
  (lambda (mat)
    (let loop-row ([rows (sub1 (matrix-rows mat))] [ls-sum '()])
      (if (>= rows 0)
          (loop-row (sub1 rows)
            (cons
              (let loop-col ([cols (sub1 (matrix-cols mat))] [sum 0])
                (if (>= cols 0)
                    (loop-col (sub1 cols) (+ (matrix-ref mat rows cols) sum))
                    sum))
              ls-sum))
          ls-sum))))

;B) sum-cols procedure: takes a matrix mat and returns a list of the sums of
; each column in the matrix (starting from the first column).

(define sum-cols
  (lambda (mat)
    (let loop-col ([cols (sub1 (matrix-cols mat))] [ls-sum '()])
      (if (>= cols 0)
          (loop-col (sub1 cols)
            (cons
              (let loop-row ([rows (sub1 (matrix-rows mat))] [sum 0])
                (if (>= rows 0)
                    (loop-row (sub1 rows) (+ (matrix-ref mat rows cols) sum))
                    sum))
              ls-sum))
          ls-sum))))

;C) sum-diag+ procedure: takes a square matrix mat and returns the sum of each
; element in the main diagonal of the matrix (the main diagonal starts at the
; upper-left element and ends at the lower-right element).

(define sum-diag+
  (lambda (mat)
    (let ([rows-end (matrix-rows mat)])
      (let loop ([row 0] [col 0] [sum 0])
        (if (< row rows-end)
            (loop (add1 row) (add1 col) (+ (matrix-ref mat row col) sum))
            sum)))))

;D) sum-diag- procedure: takes a square matrix mat and returns the sum of each
; element in the non-main diagonal of the matrix (the non-mail diagonal starts
; at the upper right element and ends at the lower left element).

(define sum-diag-
  (lambda (mat)
    (let ([cols-end (matrix-cols mat)])
      (let loop ([row (sub1 (matrix-rows mat))] [col 0] [sum 0])
        (if (< col cols-end)
            (loop (sub1 row) (add1 col) (+ (matrix-ref mat row col) sum))
            sum)))))

;E) magic-square? procedure: takes a squared matrix mat and returns #t if and
; only if the matrix is a magic square.

(define mat4
  (vov->matrix
    '#(#(4 14 15 0) #(9 7 6 12) #(5 11 10 8) #(16 2 3 13))))

(define magic-square?
  (lambda (mat)
    (if (equal? (sum-diag+ mat) (sum-diag- mat))
        (let loop ([rows (sum-rows mat)] [cols (sum-cols mat)])
          (cond
            [(null? rows) #t]
            [(equal? (sum-diag+ mat) (and (car rows) (car cols)))
             (loop (cdr rows) (cdr cols))]
            [else #f]))
        #f)))
