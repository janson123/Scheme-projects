
; adds a l for each unary value in the list.

(define u-count
  (lambda (ls)
    (cond
      [(null? ls) '() ]
      [else (cons 'l (u-count (cdr ls)))])))
; adds one to a lists of unary

(define u-add-one
  (lambda (ls)
    (cond
      [(null? ls) (cons 'l '() )]
      [else (cons 'l (u-add-one (cdr ls)))])))

; adds two lists containing unary 

(define u-add
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (u-add (cdr ls1) ls2))])))


; subtracts the smaller unary list from the bigger one.

(define u-subtract
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [else (u-subtract (cdr ls1) (cdr ls2))])))

; takes two lists containing unary, and says which one is bigger

(define u-gte?
  (lambda (ls1 ls2)
    (cond
      [(equal? ls1 ls2) #t]
      [(null? ls2) #t]
      [(null? ls1) #f]
      [else (u-gte? (cdr ls1) (cdr ls2))])))


; tells if the unary list is even

(define u-even?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(null? (cdr ls)) #f]
      [else (u-even? (cddr ls))])))


(define u-odd?
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [(null? (cdr ls)) #t]
      [else (u-odd? (cddr ls))])))


(define u-double
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [else (cons (car ls) (cons (car ls) (u-double (cdr ls))))])))


(define u-multiply
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) '()]
      [else (u-add ls1 (u-multiply ls1 (cdr ls2)))])))


(define u-half
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) (cdr ls)]
      [else (cons 'l (u-half (cddr ls)))])))


(define u-quotient
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) '()]
      [(u-gte? ls2 ls1) '()]
      [else (cons 'l (u-quotient (u-subtract ls1 ls2) ls2))])))


(define u-abs-dif
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [(null? ls1) ls2]
      [(equal? ls1 ls2) '()]
      [else (u-abs-dif (cdr ls1) (cdr ls2))])))


;A) u-lt? procedure: that takes two lists ls1 and ls2 each representing a number
; in unary and returns #t if and only if the number represented by ls1 is
; less than the number represented by ls2.

(define u-lt?
  (lambda (ls1 ls2)
    (cond
      [(equal? ls1 ls2) #f]
      [(null? ls1) #t]
      [(null? ls2) #f]
      [else (u-lt? (cdr ls1) (cdr ls2))])))

;B) u-min procedure:  takes two lists ls1 and ls2 each representing a
; nonnegative integer in unary and returns the list that represents
; the mininum of the two numbers.

(define u-min
  (lambda (ls1 ls2)
    (cond
      [(equal? #t (u-min-helper ls1 ls2)) ls1]
      [(equal? #f (u-min-helper ls1 ls2)) ls2]
      [else 'error])))

; says whether or not the second list is bigger

(define u-min-helper
  (lambda (ls1 ls2)
    (cond
      [(equal? ls1 ls2) #t]
      [(null? ls2) #f]
      [(null? ls1) #t]
      [else (u-min-helper (cdr ls1) (cdr ls2))])))


;C) u-pow-two procedure: takes one list ls representing a nonnegative number
; in unary, and returns the list representing the number two to the
; power of the number.

(define u-pow-two
  (lambda (ls)
    (u-p-h ls '(l))))


(define u-p-h
  (lambda (ls result)
    (if (null? ls) result
        (u-p-h (cdr ls) (u-multiply '(l l) result)))))



;u-rem procedure: takes two lists ls1 and ls2 each representing a number
; in unary, with ls2 different from zero, and returns the list representing
; the remainder of the two numbers.

(define u-rem
  (lambda (ls1 ls2)
    (define u-greater
      (lambda (ls1 ls2)
        (cond
          [(null? ls1) #f]
          [(null? ls2) #t]
          [else (u-greater (cdr ls1) (cdr ls2))])))
    (define u-sub
      (lambda (ls1 ls2)
        (cond
          [(null? ls1) #f]
          [(null? ls2) ls1]
          [else (u-sub (cdr ls1) (cdr ls2))])))

    (if (u-greater ls1 ls2)
        (u-rem (u-sub ls1 ls2) ls2)
        ls1)))


; u-log procedure: takes a list ls representing a number in unary and returns
; the list representing the integer part of the logarithm in
; base two of the number ls.

(define u-log
  (lambda (ls)
    (define u-half
      (lambda (ls)
        (cond
          [(null? ls) ls]
          [(null? (cdr ls)) (cdr ls)]
          [else (cons 'l (u-half (cddr ls)))])))

    (let loop ([lst ls] [acc '()])
      (if (equal? lst '(l))
          acc
          (loop (u-half lst) (cons 'l acc))))))



; u-max procedure: takes a list of lists ls each representing a number in
; unary and returns the list in ls representing the maximum number.

(define u-max
  (lambda (ls)
    (define u-greater
      (lambda (ls1 ls2)
        (cond
          [(null? ls1) #f]
          [(null? ls2) #t]
          [else (u-greater (cdr ls1) (cdr ls2))])))

    (let loop ([lst ls] [max-ls '()])
      (cond
        [(null? lst) max-ls]
        [(u-greater (car lst) max-ls) (loop (cdr lst) (car lst))]
        [else (loop (cdr lst) max-ls)]))))



(define decreasing ; makes a list of unary numbers in decreasing order.
  (lambda (ls)
    (define u-gte?
      (lambda (ls1 ls2)
        (cond
          [(equal? ls1 ls2) #t]
          [(null? ls2) #t]
          [(null? ls1) #f]
          [else (u-gte? (cdr ls1) (cdr ls2))])))
    (sort u-gte? ls)))

; Define a procedure uiota that takes one list ls representing
; the number n in unary, and returns a list of lists each representing
; the numbers 0, 1, 2,..., n - 1.

(define uiota
  (lambda (ls)
    (define helper
      (lambda (ls acc)
        (cond
          [(null? ls) acc]
          [else (helper (cdr ls) (cons (cdr ls) acc))])))
    (helper ls '())))