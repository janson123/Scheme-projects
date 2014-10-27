
(define vector-sum
  (lambda (vec)
    (let loop ([i 0] [acc 0])
      (if (= i (vector-length vec))
          acc
          (loop (add1 i) (+ (vector-ref vec i) acc))))))


; returns the index value

(define vector-member?
  (lambda (x v)
    (let loop ([i 0])
      (cond
        [(= i (vector-length v)) #f]
        [(equal? x (vector-ref v i)) i]
        [else (loop (add1 i))]))))


(define vector-add1
  (lambda (vec)
    (let ([newvec (make-vector (vector-length vec))])
      (let loop ([i 0])
        (if (< i (vector-length vec))
            (begin
              (vector-set! newvec i (add1 (vector-ref vec i)))
              (loop (add1 i)))))
      newvec)))

(define vector-add1-same
  (lambda (vec)
    (let ([n (vector-length vec)])
      (let loop ([i 0])
        (if (< i n)
            (begin
              (vector-set! vec i (add1 (vector-ref vec i)))
              (loop (add1 i))))))))



(define vector-reverse
  (lambda (vec)
    (let* ([n (vector-length vec)] [new (make-vector n)])
        (let loop ([i 0] [j (sub1 n)])
          (when (< i n)
                (vector-set! new i (vector-ref vec j))
                (loop (add1 i) (sub1 j))))
        new)))


(define vector-max
  (lambda (vec)
    (let loop ([acc 0] [max-value (vector-ref vec 0)])
      (cond
        [(= acc (vector-length vec)) max-value]
        [(> (vector-ref vec acc) max-value)
                         (loop (add1 acc) (vector-ref vec acc))]
        [else (loop (add1 acc) max-value)]))))


; returns the index position of the last occurrence of x in a vector.

(define index-of-last
  (lambda (vec x)
    (let loop ([i (sub1 (vector-length vec))])
      (cond
        [(negative? i) #f]
        [(equal? (vector-ref vec i) x) i]
        [else (loop (sub1 i))]))))



;sum-index-list procedure: takes a vector of numbers vec, a list ls of valid
;indices for vec, and returns the sum of the elements in vec with indices in ls.

(define sum-index-list
  (lambda (vec ls)
    (define helper
      (lambda (vec ls acc z)
        (cond
          [(null? ls) acc]
          [(equal? (car ls) z) (helper vec (cdr ls)
                                 (+ (vector-ref vec z) acc) (add1 z))]
          [else (helper vec ls acc (add1 z))])))
    (helper vec ls 0 0)))

;sum-index-vector procedure: takes a vector of numbers vec, a vector v of
; valid indices for vec, and returns the sum of the elements in vec
; with indices in v.

(define sum-index-vector
  (lambda (vec v)
    (let ([n (vector-length v)])
      (let help ([acc 0])
        (if (= n acc)
            0
            (+ (vector-ref vec (vector-ref v acc)) (help (add1 acc))))))))


;vector-combine! procedure: takes a vector v1, a procedure of two arguments,
; and a vector v2 and destructively updates v1 to contain the result of applying
; proc to corresponding elements of v1 and v2. That is, element i of v1 is
; overwritten by the result of applying proc to the original contents of element
; i of v1 and element i of v2. Note that the procedure mutates vector v1 in
; place, but does not mutate v2. It does not return a new vector.

(define vector-combine!
  (lambda (v1 proc v2)
    (let loop ([i 0])
      (if (< i (vector-length v1))
          (begin
            (vector-set! v1 i (proc (vector-ref v1 i) (vector-ref v2 i)))
            (loop (add1 i)))))))


;index-of-max procedure: takes a vector of numbers, vec, and a vector,
; index-vector, consisting only of valid indices within vec, and returns the
; index in index-vectors at which vec has the largest value. If there is more
; than one element with the largest value, return the smallest index.

(define index-of-max
  (lambda (vec index-vector)
    (let ([vec-len (vector-length index-vector)])
      (let help ([acc 0] [ref (vector-ref index-vector 0)])
        (cond
          [(= acc vec-len) ref]
          [(< (vector-ref vec ref)
             (vector-ref vec (vector-ref index-vector acc)))
           (help (add1 acc) (vector-ref index-vector acc))]
          [else (help (add1 acc) ref)])))))