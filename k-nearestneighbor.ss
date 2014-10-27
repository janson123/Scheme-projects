


(define euclidean-distance
       (lambda (p q)
         (sqrt
           (+ (expt (- (car p) (car q)) 2) (expt (- (cadr p) (cadr q)) 2)))))

(define training-data
       '((d (1 8)) (d (2 9)) (d (8 10)) (d (4 2)) (r (1 3)) (r (2 1)) (r (4 8))
         (r (6 4)) (d (7 3)) (r (1 5)) (d (1 9)) (d (6 2)) (r (10 9)) (d (7 7))
          (d (5 11)) (r (1 1)) (r (0 9)) (r (12 12)) (r (20 30))))

;taxicab-distance procedure: takes two points and returns the sum of the
; absolute differences of their coordinates.

(define taxicab-distance
  (lambda (pt1 pt2)
    (+ (abs (- (car pt1) (car pt2))) (abs (- (cadr pt1) (cadr pt2))))))

;knn procedure: takes a non-empty list of labeled data and a non-empty list
; of labels, and returns the label that occurs most frequently. If there is more
; than one such label, then it doesn't matter which one your program returns.


(define knn
  (lambda (k pt dist-type ls)
    (define sortedknn
      (lambda (ls)
        (cond
          [(null? ls) ls]
          [(null? (cdr ls)) ls]
          [else (quicksort (lambda (dist1 dist2)
                                (< (car dist1) (car dist2))) ls)])))
    (define knn-helper
      (lambda (pt dist-type ls)
        (map (lambda (e) (cons (dist-type pt (cadr e)) e)) ls)))
    (let google ([new (sortedknn (knn-helper pt dist-type ls))] [index k])
      (cond
        [(zero? index) '()]
        [else (cons (cdar new) (google (cdr new) (sub1 index)))]))))



;majority procedure: takes a non-empty list of labeled data and a non-empty
; list of labels, and returns the label that occurs most frequently. If there
; is more than one such label, then it doesn't matter which one
; your program returns.


(define majority
  (lambda (data ls)
    (define helper1
      (lambda (data ls)
        (let google ([data1 data] [ls1 ls])
           (if (null? ls1) '()
             (cons (let loop ([data2 data1] [ls2 (car ls1)] [acc 0])
                     (cond
                       [(null? data2) acc]
                  [(equal? (caar data2) ls2) (loop (cdr data2) ls2 (add1 acc))]
                       [else (loop (cdr data2) ls2 acc)]))
               (google data1 (cdr ls1)))))))
    (let juice ([hats (helper1 data ls)] [voodoo ls])
      (cond
        [(null? (cdr voodoo)) (car voodoo)]
        [(> (car hats) (cadr hats)) (juice (cons (car hats) (cddr hats))
                                           (cons (car voodoo) (cddr voodoo)))]
        [else (juice (cdr hats) (cdr voodoo))]))))



;classify procedure: takes an odd positive integer k, the coordinates of a
; new house, a procedure that measures the distance between two houses, a list
; of classes, and some training data, and applies the K-Nearest Neighbors
; algorithm to predict the party affiliation of the new house.



(define classify
  (lambda (k pt dist-type pt2 ls)
    (majority (knn k pt dist-type ls) pt2)))
