;;;;;;;;;
;;1.9
;;;;;;;;;

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc (5)))))
(inc (inc (inc (6))))
(inc (inc (7)))
(inc (8))
9

;;This process is recursive.

;;;;;;;

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;;This process is iterative


;;;;;;;;;
;;1.10
;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
						     (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
						     (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
				 (A 0 (A 0 (A 0 (A 0
						   (A 0 (A 0
							   (A 0
							      (A 0
								 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
				 (A 0
				    (A 0
				       (A 0
					  (A 0
					     (A 0
						(A 0
						   (A 0
						      (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
						     (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
						     (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
				 (A 0 (A 0 (A 0 (A 0
						   (A 0 (A 0
							   (A 0
							      (A 0
								 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0
				 (A 0
				    (A 0
				       (A 0
					  (A 0
					     (A 0
						(A 0
						   (A 0
						      (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(define (f n) (A 0 n))
;;(f n) computes 2n

(define (g n) (A 1 n))
;;(g n) computes 2^n
(g 3)

(define (h n) (A 2 n))
;;(h n) computes 2^(h(n-1))
;;(h 0) = 0
;;(h 1) = 2

;;;;;;;;;
;;1.11
;;;;;;;;;

;;recursive procedure
(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

;;iterative procedure
(define (f-it n)
  (define (f-iter a b c count)
    (cond ((= count 0) c)
	  (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))


;;;;;;;;;
;;1.12
;;;;;;;;;

(define (pascal row col)
  (cond ((or (= col 0) (= col row)) 1)
	((or (< col 0) (> col row)) 0)
	(else (+ (pascal (- row 1) (- col 1))
		 (pascal (- row 1) col)))))

;;;;;;;;;
;;1.16
;;;;;;;;;

(define (fast-exp b n)
  (define (exp-iter a b n)
    (define (even? n)
      (= (remainder n 2) 0))
    (define (square x) (* x x))
    (cond ((= n 0) a)
	  ((even? n) (exp-iter a (square b) (/ n 2)))
	  (else (exp-iter (* a b) b (- n 1)))))
  (exp-iter 1 b n))

;;;;;;;;;
;;1.17
;;;;;;;;;

(define (fast-mul a b)
  (define (double x)
    (* 2 x))
  (define (halve x)
    (/ x 2))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (fast-mul (double a) (halve b)))
	(else (+ a (fast-mul a (- b 1))))))

;;;;;;;;;
;;1.18
;;;;;;;;;

(define (fast-mul a b)
  (define (mul-iter c a b)
    (define (double x)
      (* 2 x))
    (define (halve x)
      (/ x 2))
    (define (even? n)
      (= (remainder n 2) 0))
    (cond ((or (= a 0) (= b 0)) c)
	  ((even? b) (mul-iter c (double a) (halve b)))
	  (else (mul-iter (+ c a) a (- b 1)))))
  (mul-iter 0 a b))

;;;;;;;;;
;;1.19
;;;;;;;;;

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))
		   (+ (* 2 p q) (square q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;;;;;;;;;
;;1.20
;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;Applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
;;remainder is executed 4 times


;;Normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

;;blah blah...many more than applicative-order

;;;;;;;;;
;;1.21
;;;;;;;;;

(define (next n)
  (cond ((= n 2) 3)
	(else (+ n 2))))

(next 2)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 5)

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)

;;;;;;;;;
;;1.22
;;;;;;;;;

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(timed-prime-test 5)

(define (search-for-primes start num-remaining)
  (cond ((= num-remaining 0) (display "done") (newline))
	((prime? start) (display start) (newline) (search-for-primes (+ start 1) (- num-remaining 1)))
	(else (search-for-primes (+ start 1) num-remaining))))

(search-for-primes 1000 3)

;;;;;;;;;
;;1.27
;;;;;;;;;

(define (square x) (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (congruent? a n)
  (= (expmod a n n) (remainder a n)))

(define (fermat n)
  (define (fermat-iter start)
    (cond ((= n 1) #f)
	  ((= start n) #t)
	  ((= (remainder start n) (expmod start n n)) (fermat-iter (+ start 1)))
	  (else #f)))
  (fermat-iter 1))

(fermat 6601)

;;;;;;;;;
;;1.28
;;;;;;;;;

;; blah blah

;;;;;;;;;
;;1.29
;;;;;;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (even? n) (= (remainder n 2) 0))

(define (integral f a b n)
  (define (term count)
    (cond ((or (= count 0) (= count n)) (f (+ a (* count (/ (- b a) n)))))
	  ((even? count) (* 2 (f (+ a (* count (/ (- b a) n))))))
	  (else (* 4 (f (+ a (* count (/ (- b a) n))))))))
  (* (/ (/ (- b a) n) 3) (sum term 0 inc n)))

(integral cube 0 1.0 100000)

;;;;;;;;;
;;1.30
;;;;;;;;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;;e.g.
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)

;;;;;;;;;
;;1.31
;;;;;;;;;


;;a.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (inc a) (+ a 1))
  (product identity 1 inc n))

(factorial 5)

(define (pi n)
  (define (term count)
    (if (even? count) (/ (+ 2.0 count) (+ count 1.0))
	(/ (+ count 1.0) (+ count 2.0))))
  (product term 1.0 inc n))

(* 4 (pi 9000))

;;b.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(product identity 1 inc 900)

;;tail recursive
(define (accumulate combiner null-value term a next b)
  (define (accm-iter a result)
    (if (> a b)
	result
	(accm-iter (next a) (combiner (term a) result))))
  (accm-iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a inc b))

(define (sum term a next b)
  (accumulate + 0 term a inc b))

(sum identity 0 inc 5)

;;linear recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a inc b))

;;;;;;;;;
;;1.33
;;;;;;;;;

;;a.
(define (filtered-accumulate combiner null-value term a next b filt?)
  (define (accm-iter a result)
    (if (> a b)
	result
	(accm-iter (next a) (if (filt? a) (combiner (term a) result)
				result))))
  (accm-iter a null-value))

(define (sum-primes a b)
  (filtered-accumulate + 0 identity a inc b prime?))

(sum-primes 1 9)

;;b.

(define (product-coprimes n)
  (define (coprime? a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 identity 1 inc n coprime?))

(product-coprimes 9)

(define (f g)
  (g 2))

;;;;;;;;;
;;1.35
;;;;;;;;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;golden ratio phi
(/ (+ 1 (sqrt 5)) 2)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;;;;;;;;;
;;1.36
;;;;;;;;;

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(let ((a (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)))
  (display "a = ")
  (display (/ (log 1000) (log a))))

(define (average x y)
  (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

;;;;;;;;;
;;1.37
;;;;;;;;;

;;a.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (cont-frac n d k)
  (define (next i) (+ i 1))
  (define (term i) i)
  (define (combiner i frac)
    (/ (n i) (+ (d i) frac)))
  (accumulate combiner 0 term 1 next k))

;;golden ratio phi
(define phi (/ (+ 1 (sqrt 5)) 2))

(define (print-phi-inv)
(display "phi = ")
(display (/ 1 phi)))

(print-phi-inv)

(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 200000))

;;b.

(define (accumulate combiner null-value term a next b)
  (define (accm-iter a result)
    (if (> a b)
	result
	(accm-iter (next a) (combiner (term a) result))))
  (accm-iter a null-value))

(define (cont-frac n d k)
  (define (next i) (+ i 1))
  (define (term i) i)
  (define (combiner i frac)
    (/ (n (+ (- k i) 1)) (+ (d (+ (- k i) 1)) frac)))
  (accumulate combiner 0 term 1 next k))

(display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12))

;;;;;;;;;
;;1.38
;;;;;;;;;

(+ (cont-frac (lambda (i) 1.0) (lambda (i)
				 (if (= (remainder i 3) 2)
				     (* 2 (+ 1 (quotient i 3)))
				     1)) 100)
   2)

;;;;;;;;;
;;1.39
;;;;;;;;;
(define (cont-frac n d k)
  (define (next i) (+ i 1))
  (define (term i) i)
  (define (combiner i frac)
    (/ (n (+ (- k i) 1)) (- (d (+ (- k i) 1)) frac)))
  (accumulate combiner 0 term 1 next k))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (n) (if (= n 1) x (square x))) (lambda (n) (- (* 2 n) 1))
	     k))

(tan-cf 3.14 8)

;;;;;;;;;
;;1.40
;;;;;;;;;
(define (cube x) (* x x x))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (deriv g)
(define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)

;;;;;;;;;
;;1.41
;;;;;;;;;

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))

(((double (double double)) inc) 5)

;;;;;;;;;
;;1.42
;;;;;;;;;

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;;;;;;;;;
;;1.43
;;;;;;;;;

(define (identity x) x)
(define (square x) (* x x))
(define (repeated f n)
  (cond ((= n 0) identity)
	((= n 1) f)
	(else (compose f (repeated f (- n 1))))))

((repeated square 2) 5)

;;;;;;;;;
;;1.44
;;;;;;;;;

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

;;;;;;;;;
;;1.45
;;;;;;;;;

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			    average-damp
			    1.0))

(define (fast-exp b n)
  (define (exp-iter a b n)
    (define (even? n)
      (= (remainder n 2) 0))
    (define (square x) (* x x))
    (cond ((= n 0) a)
	  ((even? n) (exp-iter a (square b) (/ n 2)))
	  (else (exp-iter (* a b) b (- n 1)))))
  (exp-iter 1 b n))

(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (fast-exp y (- n 1))))
			    (repeated average-damp (quotient n 2))
			    1.0))

(nth-root 625 4)

;;;;;;;;;
;;1.46
;;;;;;;;;

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
	guess
	((iterative-improve good-enough? improve) (improve guess)))))

;;old version
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;new version
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess )
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

;;old version
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;new version

(define (sqrt x)
  (define (close-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) 1.0))

