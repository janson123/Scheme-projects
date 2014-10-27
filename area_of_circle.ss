

;circle-y procedure: takes a number between 0 and 1 which represents the
; x-coordinate of a point on the unit-circle (a circle of radius 1 centered
; at the origin) and returns the point's corresponding y-coordinate
; in the first quadrant.

(define circle-y
  (lambda (x)
    (sqrt (- 1 (expt x 2)))))


;J) split-equally procedure: takes a positive integer n and returns
; a partition of (0 1) into n parts.

(define split-equally
  (lambda (n)
    (cond
      [(= n 0) (list '0)]
      [else (list-builder2 n (/ 1 n) '(1))])))

(define list-builder2
  (lambda (n increment result)
    (cond
      [(= n 0) result]
      [else (list-builder2 (sub1 n) increment
                        (cons (* (- n 1.0) increment) result))])))





;area-circle procedure: takes a nonnegative integer and returns an
; approximation of pi by calculating four times the total area of the
; trapezoids with points on the circle

(define area-circle
  (lambda (n)
   (cond
    [(= n 0) '0]
    [else (* 4 (addition (area (sub-lists (split-equally n)) n '()) 0))])))

(define sub-lists
  (lambda (ls)
    (map (lambda (x) (circle-y x)) ls)))

(define area
  (lambda (ls n result)
    (cond
      [(null? (cdr ls)) result]
      [else (area (cdr ls) n
                (cons (area-trapezoid (car ls) (cadr ls) (/ 1 n)) result))])))

(define addition
  (lambda (ls result)
    (cond
      [(null? ls) result]
      [else (addition (cdr ls) (+ (car ls) result))])))