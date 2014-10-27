; inserts variable s at the end of a list.

(define insert-at-the-end
  (lambda (s ls)
    (cond
      [(null? ls) (cons s '())]
      [else (cons (car ls) (insert-at-the-end s (cdr ls)))])))

; zero-one takes non-negative integer and returns 01 n times

(define zero-one
  (lambda (n)
    (cond
      [(= n 0) '()]
      [else (cons 0 (cons 1 (zero-one (sub1 n))))])))


; (iterate 3 add1 0)
; 3

(define iterate
  (lambda (n proc x)
    (cond
      [(zero? n) x]
      [else (iterate (sub1 n) proc (proc x))])))


; adds up the values in a list

(define list-sum
  (lambda (ls)
    (let loop ([lst ls] [acc 0])
      (if (null? lst)
          acc
          (loop (cdr lst) (+ (car lst) acc))))))


; finds the minimum number in a list

(define find-min
  (lambda (ls)
    (let loop ([lst ls] [acc (car ls)])
      (cond
        [(null? lst) acc]
        [(< acc (car lst)) (loop (cdr lst) acc)]
        [else (loop (cdr lst) (car lst))]))))

; finds the max number in a list

(define find-max
  (lambda (ls)
    (trace-let loop ([lst ls] [acc 0])
      (cond
        [(null? lst) acc]
        [(> acc (car lst)) (loop (cdr lst) acc)]
        [else (loop (cdr lst) (car lst))]))))


; returns the index value of the first occurance of the item

(define find-first
  (lambda (item ls)
    (cond
      [(null? ls) #f]
      [(equal? item (car ls)) 0]
      [else (+ 1 (find-first item (cdr ls)))])))

; basically the same as find-first

(define locate
  (lambda (x ls)
    (let loop ([item x] [lst ls] [acc 0])
      (cond
        [(null? lst) #f]
        [(equal? item (car lst)) acc]
        [else (loop item (cdr lst) (add1 acc))]))))



(define rotate
  (lambda (ls)
    (if (or (null? (cdr ls)) (null? ls))
        ls
      (cons (car (rotate (cdr ls))) (cons (car ls) (cdr (rotate (cdr ls))))))))

; Define a procedure take which takes a list ls and a number n, and returns a
; list in the same order as the original list,
; but containing only the first n elements.

(define take
  (lambda (ls n)
    (if (or (null? ls) (zero? n))
        '()
        (cons (car ls) (take (cdr ls) (sub1 n))))))

; Define a procedure drop which takes a list ls and a number n, and returns a
; list in the same order as the original list, but without the last n elements.


(define drop
  (lambda (ls n)
    (if (or (null? ls) (zero? n))
        ls
        (drop (cdr ls) (sub1 n)))))



; Using the above two procedures, define a procedure subseq which takes a
; list ls, and two indices n and m, and returns
; the sublist between those two indices.

(define subseq
  (lambda (ls n m)
    (take (drop ls n) (- m n))))

; Define a procedure union which takes a list ls and returns the union of the
; values contained in that list.

; ~ (union '(a b c d) '(c d e f))
;(a b c d e f)


(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(member (car ls1) ls2)
          (union (cdr ls1) ls2)]
      (else
       (cons (car ls1) (union (cdr ls1) ls2))))))



; We will write a procedure that outputs from any given list
; the element at index 4. If 4 is not a valid index of the given list,
; it will output the last element.



;(define ex-ls '(1 2 3 4 5 6 7))

(define return-4-index
  (lambda (ls)
    (let loop ( [i 0] [lsx ls] )
      (if (null? (cdr lsx))
          (car lsx)
          (if (= i 4)
              (car lsx)
              (loop (add1 i) (cdr lsx)))))))


;A) crunch-front procedure: this takes a list as an input, compares the first
; two elements in the list, and then returns a new list wih the smaller of the
; two compared consed to the front of the rest of the original
; list as an output.

(define crunch-front
  (lambda (ls)
    (cond
      [ (< (car ls) (car(cdr ls))) (cons (car ls) (cddr ls))]
      [ (> (car ls) (car(cdr ls))) (cons (car(cdr ls)) (cddr ls))]
      [else 'oops])))


;double-symbol procedure: takes a list and a symbol, and returns a list where
; if the original list contained the symbol its frequency is doubled per
; occurance in the original list.

(define double-symbol
  (lambda (symbol ls)
    (cond
      [(null? ls) ls]
      [(equal? symbol (car ls))
               (cons symbol (cons symbol (double-symbol symbol (cdr ls))))]
      [else (cons (car ls) (double-symbol symbol (cdr ls)))])))

;find-replace procedure: this takes two symbols and a list, and returns a
; list where each occurence of the first symbol is replaced by the second.

(define find-replace
  (lambda (f s ls)
    (cond
      [(null? ls) ls]
      [(equal? f (car ls)) (cons s (find-replace f s (cdr ls)))]
      [else (cons (car ls) (find-replace f s (cdr ls)))])))

;index-of procedure: this takes a symbol and a list, and returns the index of
; the first occurence of the symbol in the list.

(define index-of
  (lambda (symbol ls)
    (cond
      [(equal? symbol (car ls)) 0]
      [else (+ 1 (index-of symbol (cdr ls)))])))

;remove-symbol procedure: this takes a symbol and a list, and returns a list
; of the same elements as before except for the symbol that has been removed.

(define remove-symbol
  (lambda (symbol ls)
    (cond
      [(null? ls) ls]
      [(equal? symbol (car ls)) (remove-symbol symbol (cdr ls))]
      [else (cons (car ls) (remove-symbol symbol (cdr ls)))])))




;pick-pile procedure: this takes two lists, and returns the larger of the two
; lists.

(define pick-pile
  (lambda (ls1 ls2)
    (cond
      [(equal? ls1 ls2) ls1]
      [(equal? #t (u-gte? ls1 ls2)) ls1]
      [else ls2])))

;pick-tray procedure: this takes two trays (lists) with each list
; containing two "piles" in each of the trays and then returns the larger tray.

(define h1
  (lambda (ls)
    (car ls)))

(define h2
  (lambda (ls)
    (cadr ls)))

(define list_maker
  (lambda (h1 h2)
    (cond
      [(null? h2) h1]
      [else (cons (car h2) (list_maker h1 (cdr h2)))])))


(define pick-tray
  (lambda (ls1 ls2)
    (if (u-gte? (list_maker (h1 ls1) (h2 ls1)) (list_maker (h1 ls2)
                                                      (h2 ls2))) ls1 ls2)))

; append procedure: this takes two lists, and returns one list containing the
; elements of the first list then the second list.

(define append
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

; member? procedure: this takes an item and a list, and returns #t if the item
; appears in the list.

(define member?
  (lambda (item ls)
    (cond
      [(null? ls) #f]
      [(equal? (car ls) item) #t]
      [else (member? item (cdr ls))])))



;intersection procedure: this takes two lists, and returns a list of elements
; that were in both lists.

(define intersection
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(member? (car ls1) ls2) (cons (car ls1) (intersection (cdr ls1) ls2))]
      [else (intersection (cdr ls1) ls2)])))



;prefix? procedure: takes two lists and returns #t if the first list is a
; prefix of the second list, and #f if it is not.

(define prefix?
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) #t]
      [(equal? ls1 ls2) #t]
      [(null? ls2) #f]
      [(equal? (car ls1) (car ls2)) (prefix? (cdr ls1) (cdr ls2))]
      [else #f])))

;suffix? procedure: takes two lists and returns #t if the first list is a
; suffix of the second list, and #f otherwise.

(define suffix?
  (lambda (ls1 ls2)
    (cond
      [(equal? ls1 '()) #t]
      [(null? ls2) #f]
      [(equal? ls1 ls2) #t]
      [else (suffix? ls1 (cdr ls2))])))

;fuse-fragments procedure: takes two lists representing DNA sequences,
; locates the area of overlap, and returns the result of joining the two
; sequences together.


(define fuse-fragments
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [(null? ls1) ls2]
      [(prefix? ls1 ls2) ls2]
      [else (cons (car ls1) (fuse-fragments (cdr ls1) ls2))])))


;trim-when procedure: takes a predicate and a list, and removes elements from
; the list if it satisfies the predicate. It stops and returns the list once
; it reaches an element that doesn't satisfy the predicate.

(define trim-when
  (lambda (x ls)
    (cond
      [(x (car ls)) (trim-when x (cdr ls))]
      [else ls])))


