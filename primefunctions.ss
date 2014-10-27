
;prime? procedure:  takes an integer greater than or equal to two and
; returns #t if and only if the number is a prime.

(define prime?
  (lambda (n)
    (define helper
      (lambda (x n)
        (cond
          [(> (* x x) n) #t]
          [(equal? '0 (mod n x)) #f]
          [else (helper (+ x 2) n)])))
    (cond
      [(or (equal? n '0) (equal? n '1)) #f]
      [(equal? n '2) #t]
      [(even? n) #f]
      [else (helper 3 n)])))

;primes? procedure: takes an integer n and returns a list of the first
; n prime numbers.

(define primes
  (lambda (n)
    (define helper
      (lambda (n counter list-counter)
        (cond
          [(= n list-counter) '()]
          [(prime? counter) (cons counter (helper n (add1 counter)
                                                   (add1 list-counter)))]
          [else (helper n (add1 counter) list-counter)])))
    (helper n 0 0)))








;cong? procedure: takes two nonnegative integers n and m and a positive
; integer clock-size and returns #t if and only if the two numbers are
; the same in a clock of size clock-size.


(define cong?
  (lambda (x y clock-size)
    (zero? (mod (- y x) clock-size))))


;exp-clock procedure: takes a positive integer base, a nonnegative integer
; n and a positve integer clock-size and returns the result of raising
; the integer base to the power n in a clock of size clock-size.


(define exp-clock
  (lambda (base n clock-size)
    (mod (expt base n) clock-size)))


;fermat-test procedure: takes two numbers base and n with base less than
; n and tests whether n is likely to be prime or not.


(define fermat-test
  (lambda (base n)
   (equal? (exp-clock base n n) base)))

;pi-leibniz procedure: takes a positive value E and returns the
; approximation of pi with an error less than E.

(define pi-leibniz
  (lambda (E)
    (let help ([sum 0] [denom 1] [z 0])
      (cond
        [(< (/ 1 denom) E) (* 4 sum)]
        [else (help (+ sum (/ (expt -1 z) (+ (* 2.0 z) 1)))
                                                   (+ 1 denom) (add1 z))]))))

