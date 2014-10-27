
;insert-in-order procedure: takes a relation, an item x, and a list of
; elements that are sorted by the given relation, and returns the result
; of inserting x into the correct location in the given list such that
; the result is in sorted order.

(define insert-in-order
  (lambda (relation x ls)
    (cond
      [(null? ls) (list x)]
      [(relation x (car ls)) (cons x ls)]
      [else (cons (car ls) (insert-in-order relation x (cdr ls)))])))

;insertion-sort procedure: takes a relation and a list of elements, and
; returns the result of sorting the list according to the given relation.

(define insertion-sort
  (lambda (relation ls)
 (define helper
   (lambda (relation ls acc)
     (if (null? ls) acc
      (helper relation (cdr ls)
                      (insert-in-order relation (car ls) acc)))))
    (helper relation ls '())))











;merge-adjacent-sequences procedure: takes a binary relation rel? and a list
; of the form returned by group-sorted-sequences and returns the list after
; merging adjacent lists together.

(define merge-adjacent-sequences
  (lambda (rel? ls)
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) ls]
      [else (cons
              (let helper ([ls1 (car ls)] [ls2 (cadr ls)])
                (cond
                  [(null? ls1) ls2]
                  [(null? ls2) ls1]
                  [(rel? (car ls1) (car ls2))
                                       (cons (car ls1) (helper (cdr ls1) ls2))]
                  [else (cons (car ls2) (helper ls1 (cdr ls2)))]))

       (merge-adjacent-sequences rel? (cddr ls)))])))



;merge-sort procedure: takes a binary relation rel? and and a list ls and
; returns the result of sorting the elements in ls according to rel? using
; the Mergesort algorithm described above.


(define merge-sort
  (lambda (rel? ls)
    (if (null? ls)
             ls
        (let help ([hats (gss rel? ls)])
          (if (null? (cdr hats))
              (car hats)
              (help (merge-adjacent-sequences rel? hats)))))))

(define gss
      (lambda (rel? ls)
        (cond
          [(null? (cdr ls)) (cons ls '())]
          [else (let ([ans (gss rel? (cdr ls))])
              (if (rel? (car ls) (caar ans))
                  (cons (cons (car ls) (car ans)) (cdr ans))
                  (cons (cons (car ls) '()) ans)))])))












;divide procedure: takes a predicate pred? and a list ls and returns a list
; with two sublists: those elements in ls that satisfy the predicate
; and those that don't.

(define divide
  (lambda (pred? ls)
    (define helper
      (lambda (pred? ls ls1 ls2)
        (cond
          [(null? ls) (list ls1 ls2)]
          [(pred? (car ls)) (helper pred? (cdr ls) (cons (car ls) ls1) ls2)]
          [else (helper pred? (cdr ls) ls1 (cons (car ls) ls2))])))
    (helper pred? ls '() '())))


;join procedure: takes a list ls1, an item goo, and another list ls2,
; and returns a list consisting of the elements of ls1 followed by goo
; followed by the elements of ls2. (Do not use append.)


(define join
  (lambda (ls1 goo ls2)
    (cond
      [(null? ls1) (cons goo ls2)]
      [else (cons (car ls1) (join (cdr ls1) goo ls2))])))


;quicksort procedure: takes a binary relation rel? and a list ls and returns
; the result of sorting the elements in ls according to rel? using the
; Quicksort algorithm described above.


(define quicksort
  (lambda (rel? ls)
    (define quicksort-helper
      (lambda (rel? ls)
        (let ([clogs (divide (lambda (x) (rel? x (car ls))) (cdr ls))])
          (join (quicksort rel? (car clogs)) (car ls)
                                  (quicksort rel? (cadr clogs))))))
    (cond
        [(null? ls) ls]
        [(null? (cdr ls)) ls]
        [else (quicksort-helper rel? ls)])))
