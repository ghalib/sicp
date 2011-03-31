;;;;;;;;;
;;2.2
;;;;;;;;;

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-segment start end)
  (cons start end))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-point x y)
  (cons x y))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (make-segment (average (x-point (start-segment segment))
			 (x-point (end-segment segment)))
		(average (y-point (start-segment segment))
			 (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define start (make-point 1.0 2.0))
(define end (make-point 7.0 1.0))
(define segment (make-segment start end))
(start-segment segment)
(end-segment segment)
(print-point (midpoint-segment segment))

;;;;;;;;;
;;2.3
;;;;;;;;;

(define (make-rect bottomleft topright)
  (if (or (> (y-point bottomleft) (y-point topright))
	  (> (x-point bottomleft) (x-point topright)))
      (display "error in coordinates")
      (cons bottomleft topright)))

(define (bottomleft rectangle)
  (car rectangle))

(define (topright rectangle)
  (cdr rectangle))

(define myrect (make-rect (make-point 1 2) (make-point 3 4)))
(bottomleft myrect)
(topright myrect)

(define (perimeter rectangle)
  (define (double x) (* 2 x))
  (+ (double (- (x-point (topright rectangle)) (x-point (bottomleft rectangle))))
     (double (- (y-point (topright rectangle)) (y-point (bottomleft rectangle))))))

(define (area rectangle)
  (* (- (x-point (topright rectangle)) (x-point (bottomleft rectangle)))
     (- (y-point (topright rectangle)) (y-point (bottomleft rectangle)))))

(area myrect)

;;;;;;;;;
;;2.4
;;;;;;;;;

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;;;;;;;;
;;2.5
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

(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (car z)
  (define (car-iter z result)
    (define (odd? n)
      (not (= (remainder n 2) 0)))
    (cond ((odd? z) result)
	  (else (car-iter (/ z 2) (+ result 1)))))
  (car-iter z 0))

(define (cdr z)
  (define (cdr-iter z result)
    (if (not (= (remainder z 3) 0))
	result
	(cdr-iter (/ z 3) (+ result 1))))
  (cdr-iter z 0))

(car (cons 15000 3))
(cdr (cons 15000 15000))

;;;;;;;;;
;;2.6
;;;;;;;;;

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;;;;;;;;
;;2.7
;;;;;;;;;

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;;;;;;;;;
;;2.8
;;;;;;;;;

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bound a) (lower-bound b))))

;;;;;;;;;
;;2.9
;;;;;;;;;

blahblah

;;;;;;;;;
;;2.10
;;;;;;;;;

(define (div-interval x y)
  (let ((lby (lower-bound y))
	(uby (upper-bound y)))
    (if (<= (* lby uby) 0)
	(error "Interval spans zero.")
	(mul-interval x
		      (make-interval (/ 1.0 uby) (/ 1.0 lby))))))

;;;;;;;;;
;;2.11
;;;;;;;;;

;;too annoying

;;;;;;;;;
;;2.12
;;;;;;;;;

;;;;;;;;;
;;2.17
;;;;;;;;;

(define (last-pair mylist)
  (let ((rest (cdr mylist)))
    (if (null? rest)
	mylist
	(last-pair rest))))

(last-pair (list 1 2 3))

;;;;;;;;;
;;2.18
;;;;;;;;;

(define (reverse mylist)
  (define (rev-iter list1 list2)
    (if (null? list2)
	list1
	(rev-iter (cons (car list2) list1) (cdr list2))))
  (rev-iter '() mylist))

(reverse (list 1 2 3))

;;;;;;;;;
;;2.19
;;;;;;;;;

;;original program

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(define (count-change amount)
  (cc amount 5))

(count-change 100)

;;new program

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define us-coins (list 5 50 10 1 25))

(cc 100 us-coins)

;;;;;;;;;
;;2.20
;;;;;;;;;

(define (same-parity comparewith . rest)
  (define (parity? x) (remainder x 2))
  (let ((desired-parity (parity? comparewith)))
    (define (parity-filter comparewith rest)
      (cond ((null? rest) '())
	    ((= (parity? (car rest)) desired-parity)
	     (cons (car rest) (parity-filter comparewith (cdr rest))))
	    (else (parity-filter comparewith (cdr rest)))))
    (cons comparewith (parity-filter comparewith rest))))

(same-parity 2 3 4 5 6 6 6 6 6 7)

;;;;;;;;;
;;2.21
;;;;;;;;;

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3 4 5))

;;;;;;;;;
;;2.22
;;;;;;;;;

;;a.
;;Because he's processing the list from head to tail but constructing the result
;;list from tail to head.

;;b.
;;He is cons'ing sublists all the time, since he cons's '() with an element to
;;start with, instead of the reverse.

;;;;;;;;;
;;2.23
;;;;;;;;;

(define (for-each proc mylist)
  (cond ((null? mylist) '())
	(else (proc (car mylist)) (for-each proc (cdr mylist)))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))

;;;;;;;;;
;;2.24
;;;;;;;;;

(list 1 (list 2 (list 3 4)))

;;;;;;;;;
;;2.25
;;;;;;;;;

(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))

(define b (list (list 7)))
(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;;;;;;;;;
;;2.26
;;;;;;;;;

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;;(1 2 3 4 5 6)

(cons x y)
;;((1 2 3) 4 5 6)

(list x y)
;;((1 2 3) (4 5 6))

;;;;;;;;;
;;2.27
;;;;;;;;;

;;old version

(define (reverse mylist)
  (define (rev-iter list1 list2)
    (if (null? list2)
	list1
	(rev-iter (cons (car list2) list1) (cdr list2))))
  (rev-iter '() mylist))

;;new version
(define (reverse mylist)
  (define (rev-iter list1 list2)
    (cond ((null? list2) list1)
	  (else (rev-iter (cons (car list2) list1) (cdr list2)))))
  (rev-iter '() mylist))

(define (deep-reverse mylist)
  (if (not (pair? mylist))
      mylist
      (reverse (map deep-reverse mylist))))

(deep-reverse (list (list (list (list 1 2)) 4 5)))
(deep-reverse (list 1 2 3)) 
(deep-reverse (list 1 (list 2 3)))

(list (list (list 1 2)) 4 5)	       

;;;;;;;;;
;;2.28
;;;;;;;;;

(define (fringe mylist)
  (cond ((null? mylist) mylist)
	((not (pair? mylist)) mylist)
	((not (pair? (car mylist))) (cons (car mylist)
					  (fringe (cdr mylist))))
	(else (append (fringe (car mylist)) (fringe (cdr mylist))))))

;;better version on Japanese SICP answer book site

(fringe (list 1 (list (list (list 2 4 5 6) 22))))

(fringe (list (list 2 3 4 54) 22))

;;;;;;;;;
;;2.29
;;;;;;;;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;;b.

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (cond ((null? branch) 0)
	  ((not (pair? struct)) struct)
	  (else (total-weight struct)))))
  
(define a (make-branch 1 2))
(define b (make-branch 1 0))
(define c (make-branch 1 2))
(define d (make-branch 1 0))
(define mob (make-mobile a b))
(define mob2 (make-mobile c d))

(define lbranch (make-branch 2 mob))
(define rbranch (make-branch 2 mob2))
(define bigmob (make-mobile lbranch rbranch))

(total-weight bigmob)
(total-weight mob)
;;c.

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (is-balanced? mobile)
  (if (or (not (pair? (right-branch mobile)))
	  (not (pair? (left-branch mobile))))
      #t
      (= (torque (left-branch mobile))
	 (torque (right-branch mobile)))))

(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (is-balanced? mobile)
	   (balanced? (right-branch mobile))
	   (balanced? (left-branch mobile)))))

(balanced? mob)

;;d.

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;these two selectors need to be changed as follows:

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

;;;;;;;;;
;;2.30
;;;;;;;;;

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (subtree)
	 (if (not (pair? subtree))
	     (* subtree subtree)
	     (square-tree subtree)))
       tree))

(square-tree (list 1 2 (list 3 4) 5))

;;;;;;;;;
;;2.31
;;;;;;;;;

(define (tree-map func tree)
  (map (lambda (subtree)
	 (if (not (pair? subtree))
	     (func subtree)
	     (tree-map func subtree)))
       tree))

(define (square x)
  (* x x))

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 2 (list 3 4) (list 5 6) 7))

;;;;;;;;;
;;2.32
;;;;;;;;;

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;example: output from (subsets '(A B)) is (() (B) (A) (A B))
;;Note that we can partition this into two lists: (() (B)) and
;;((A) (A B)) - the lists represent all subsets not including the car,
;;and all subsets including the car, respectively.
;;Notice that the second list is just the first list but with the car
;;cons'ed onto each element, followed by appending the first list to
;;the second. And indeed, this is how we construct our power set,
;;always constructing a new list by cons'ing a new element to our
;;existing subsets, then appending this new list to the existing list
;;of subsets.

;;;;;;;;;
;;2.33
;;;;;;;;;

(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;;;;;;;;;;;;;;;;;;;;;;;;

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;;;;;;;;
;;2.34
;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;;;;;;;;
;;2.35
;;;;;;;;;

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
			 (if (pair? x)
			     (count-leaves x)
			     1))
		       t)))

(count-leaves (list 1 (list 2 3) 4 5))

;;;;;;;;;
;;2.36
;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))

;;;;;;;;;
;;2.37
;;;;;;;;;

(define matrix (list '(1 2 3) '(4 5 6) '(7 8 9)))

(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(matrix-*-vector matrix '(1 1 1))

(define (transpose matrix)
  (accumulate-n cons '() matrix))

(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
	   (map (lambda (col) (dot-product row col)) cols))
	    m)))

(matrix-*-matrix matrix matrix)

;;;;;;;;;
;;2.38
;;;;;;;;;

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;;3/2

(fold-left / 1 (list 1 2 3))
;;1/6

(fold-right list '() (list 1 2 3))
;;(1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
;;(((() 1) 2) 3)

;; In order for fold-left to produce the same values as fold-right, op
;; should be associative (a common mistake is to think that commutativity is
;; what matters).

;;;;;;;;;
;;2.39
;;;;;;;;;

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse (list 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

(permutations (list 1 2 3))
(permutations (list 2 3))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

;;;;;;;;;
;;2.40
;;;;;;;;;

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pairs 6)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)

;;;;;;;;;
;;2.41
;;;;;;;;;

(define (unique-triples n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
		    (map (lambda (k) (list i j k))
			 (enumerate-interval 1 (- j 1))))
		    (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))

(define (triples n s)
  (define (equal-sum? triple)
    (= (accumulate + 0 triple) s))
  (map make-triple-sum (filter equal-sum? (unique-triples n))))

(unique-triples 3)
(triples 7 12)

;;;;;;;;;
;;2.42
;;;;;;;;;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (let ((newqueen (car positions)))
    (fold-left (lambda (x y) (and x y))
		#t
		(map (lambda (queenpos)
		       (and (not (= (car newqueen) (car queenpos)))
			    (not (= (abs (- (car newqueen) (car queenpos)))
				    (abs (- (cadr newqueen) (cadr queenpos)))))))
		     (cdr positions)))))

(define (prettyprint lst)
  (cond ((null? lst) #t)
	(else (display (car lst))
	      (newline)
	      (prettyprint (cdr lst)))))

(prettyprint (queens 4))

;;;;;;;;;
;;2.43
;;;;;;;;;

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
	  (adjoin-position new-row k rest-of-queens))
	(queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;; The program in 2.42 differs in the number of times it executes
;; queen-cols. Note that here, (queen-cols k) is executed n times (where n =
;; board-size) for some k, and so for each of these n times,
;; (queen-cols (- k 1)) is executed n times, resulting in a total of
;; n^2 times, and so on. To illustrate, here are the number of times
;; for n = 8:

;; k             |8 7 6   5   4   3   2   1
;; -------------------------------------------
;; no. of times  |1 8 8^2 8^3 8^4 8^5 8^6 8^7

;; Thus the total number of times is 1 + 8 + 8^2 +...+ 8^7. Notice
;; that we have a geometric series, so we can sum it up by 
;; (1-8^8)/(1-8) = 2396745. This is 239675/8 times as many as
;; 2.42. Thus, if we assume 2.42 runs in times T, then 2.43 runs in
;; approximately (239675/8)T.

;;;;;;;;;
;;2.44
;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;;;;;;;;
;;2.45
;;;;;;;;;

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split op1 op2) painter (- n 1))))
	  (op1 painter (op2 smaller smaller))))))

;;;;;;;;;
;;2.46
;;;;;;;;;

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
	     (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
	     (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect v k)
  (make-vect (* k (xcor-vect v))
	     (* k (ycor-vect v))))

;;;;;;;;;
;;2.47
;;;;;;;;;

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge2-frame frame)
  (cddr frame))

;; The other two selectors remain the same.

;;;;;;;;;
;;2.48
;;;;;;;;;

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;;;;;;;;;
;;2.49
;;;;;;;;;
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 1.0 0.0))
(define top-right (make-vect 1.0 1.0))
(define top-left (make-vect 0.0 1.0))

;; a.

(define (draw-border frame)
  ((segments->painter (list (make-segment bottom-left bottom-right)
			   (make-segment bottom-right top-right)
			   (make-segment top-right top-left)
			   (make-segment top-left bottom-left)))
  frame))

;; b.

(define (draw-X frame)
  ((segments->painter (list (make-segment bottom-left top-right)
			    (make-segment bottom-right top-left)))
   frame))

;; c.

(define (midpoint segment)
  (make-vect (/ (+ (xcor-vect (start segment))
		   (xcor-vect (end segment)))
		2)
	     (/ (+ (ycor-vect (start segment))
		   (ycor-vect (end segment)))
		2)))

(define (draw-diamond frame)
  ((segments->painter (list (make-segment
			     (midpoint (make-segment bottom-left
						     bottom-right))
			     (midpoint (make-segment bottom-right
						     top-right)))
			    (make-segment
			     (midpoint (make-segment bottom-right
						     top-right))
			     (midpoint (make-segment top-right
						     top-left)))
			    (make-segment
			     (midpoint (make-segment top-right
						     top-left))
			     (midpoint (make-segment top-left
						     bottom-left)))
			    (make-segment
			     (midpoint (make-segment top-left
						     bottom-left))
			     (midpoint (make-segment bottom-left
						     bottom-right)))))
   frame))

;; d.

;; Can't be bothered.

;;;;;;;;;
;;2.50
;;;;;;;;;

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))	  

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)   ; new origin
		     (make-vect 0.0 0.0)   ; new end of edge1
		     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

;;;;;;;;;
;;2.51
;;;;;;;;;

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))
	  (paint-down
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))))
      (lambda (frame)
	(paint-down frame)
	(paint-up frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;;;;;;;;;
;;2.52
;;;;;;;;;

;; Would be nice if there was a picture language implementation for Scheme48.

;;;;;;;;;
;;2.53
;;;;;;;;;

(define (memq item mylist)
  (cond ((null? mylist) #f)
	((eq? item (car mylist)) mylist)
	(else (memq item (cdr mylist)))))

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;;;;;;;;;
;;2.54
;;;;;;;;;

(define (equal? list1 list2)
  (cond ((number? list1) (= list1 list2))
	((symbol? list1) (eq? list1 list2))
	((null? list1) (null? list2))
	(else (and (equal? (car list1) (car list2))
		   (equal? (cdr list1) (cdr list2))))))

;;;;;;;;;
;;2.55
;;;;;;;;;

(car ''abracadabra)

;; As explained in SICP, 'a expands to (quote a), and so ''abracadabra
;; expands to (quote (quote abracadabra)) which when evaluated becomes
;; (quote abracadabra). Hence (car (quote abracadabra)) is quote.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	(else (display "unknown expression type -- DERIV"))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(deriv '(* x y) 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;;2.56
;;;;;;;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (make-product (exponent exp)
		       (make-product (make-exponentiation (base exp)
							  (- (exponent exp)
							     1))
				     (deriv (base exp) var))))
	(else (display "unknown expression type -- DERIV"))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
	((=number? n 1) x)
	(else (list '** x n))))

(deriv '(** x 2) 'x)

(deriv '(** x 1) 'x)

;;;;;;;;;
;;2.57
;;;;;;;;;

(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '+ (cddr x))))

(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '* (cddr x))))

(deriv '(* x y (+ x 3)) 'x)

;;;;;;;;;
;;2.58
;;;;;;;;;

;; a.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list m1 '* m2))))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (cadr exp) '**)))
(define (base exponentiation)
  (car exponentiation))
(define (exponent exponentiation)
  (caddr exponentiation))
(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
	((=number? n 1) x)
	(else (list x '** n))))

;; b.

(define (augend s)
  (let ((rest (cdr (memq '+ s))))
    (if (= (length rest) 1)
	(car rest)
	rest)))

(define (addend s)
  (if (eq? (cadr s) '+)
      (car s)
      (addend (cdr s))))

(define (sum? x)
  (if (and (pair? x) (memq '+ x))
      #t
      #f))

(define (multiplier s)
  (let ((rest (cdr (memq '* s))))
    (if (= (length rest) 1)
	(car rest)
	rest)))

(define (multiplicand s)
  (if (eq? (cadr s) '*)
      (car s)
      (multiplicand (cdr s))))

(define (product? x)
  (if (and (pair? x) (memq '* x))
      #t
      #f))

;;;;;;;;;
;;2.59
;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(a b y c) '(x y z))

;;;;;;;;;
;;2.60
;;;;;;;;;

;; element-of-set? is still unchanged, thus O(n). 
;; 
;; adjoin-set as shown below can now just be a cons operation, since
;; we don't need to check if element is a member in the set or
;; not. Thus adjoin-set becomes O(1).
;;
;; union-set is also changed; it is now just an append operation, thus
;; O(1).
;;
;; intersection-set is unchanged, thus remains O(n^2).
;;
;; Maybe would use this representation if I was doing lots of adjoins
;; and unions?

(define adjoin-set cons)
(define union-set append)

;;;;;;;;;
;;2.61
;;;;;;;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

(define set '(3 4 6))
(adjoin-set 7 set)
(adjoin-set 1 set)
(adjoin-set 5 set)

;;;;;;;;;
;;2.62
;;;;;;;;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((< x2 x1)
		       (cons x2 (union-set set1 (cdr set2))))
		      ((= x1 x2) (union-set (cdr set1) set2)))))))

(define set1 '(3 4 5))
(define set2 '(1 2 3 4 5 6))
(union-set set1 set2)
(union-set '(1) '(1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;;2.63
;;;;;;;;;

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

;; a.

;; Both procedures produce the same output: (1 3 5 7 9 11)

;; b.

;; Procedure 2 is faster. This is because one of the recursive calls is
;; tail-recursive, with the resultant list being built up with one
;; cons per element, and so with a time complexity of O(n).
;;
;; Note however that procedure 1 is not tail-recursive, and uses
;; append as well as cons, thus after the left subtree is convereted
;; into a list, we still have to append it to the right subtree's
;; list, and since append is a series of cons's, we have a time
;; complexity of O(n^2).

;;;;;;;;;
;;2.64
;;;;;;;;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

;; a.

;; partial-tree recurses on n, with the base case of n = 0 returning a
;; cons of the empty list with the list of elements.
;;
;; Otherwise, it calculates the size of the left tree as
;; floor((n-1/2)), and recursively constructs the left subtree. It then
;; gets the rest of the elements to be processed from the returned
;; cons, setting this-entry to the car of these elements. It takes the
;; rest of the elements and recursively constructs the right subtree,
;; finally creating the tree with this-entry as the root, along with
;; the left and right subtrees, using the make-tree constructor. This
;; is returned along with the remaining list elements (list of the
;; empty list, if none).

;; b.

;; This will take O(n) time, where n is the number of elements in the
;; list elts. make-tree is called once for each elements of the list
;; as this-entry.

;;;;;;;;;
;;2.65
;;;;;;;;;

(define (union-set set1 set2)
  (define (union-ordered-list list1 list2)
    (cond ((null? list1) list2)
	  ((null? list2) list1)
	  (else (let ((x1 (car list1))
		      (x2 (car list2)))
		  (cond ((< x1 x2)
			 (cons x1 (union-ordered-list (cdr list1) list2)))
			((< x2 x1)
			 (cons x2 (union-ordered-list list1 (cdr list2))))
			((= x1 x2) (union-ordered-list (cdr list1)
						       list2)))))))
  (list->tree (union-ordered-list (tree->list-2 set1)
				  (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (intersection-ordered-list list1 list2)
    (if (or (null? list1) (null? list2))
	'()
	(let ((x1 (car list1))
	      (x2 (car list2)))
	  (cond ((< x1 x2) (intersection-ordered-list
			    (cdr list1)
			    list2))
		((< x2 x1 (intersection-ordered-list
			   list1
			   (cdr list2))))
		((= x1 x2 (cons x1 (intersection-ordered-list
				    (cdr list1)
				    (cdr list2)))))))))
  (list->tree (intersection-ordered-list (tree->list-2 set1)
					 (tree->list-2 set2))))

;;;;;;;;;
;;2.66
;;;;;;;;;

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((this-key (key set-of-records)))
	(cond ((= given-key this-key) this-key)
	      ((< given-key this-key)
	       (lookup given-key (left-branch set-of-records)))
	      ((> given-key this-key)
	       (lookup given-key (right-branch set-of-records)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (display "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;;2.67
;;;;;;;;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;; (a d a b b c a)

;;;;;;;;;
;;2.68
;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
	((leaf? tree)
	 (if (not (equal? symbol (symbol-leaf tree)))
	     (error "Symbol not in tree")
	     '()))
	((element-of-set? symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((element-of-set? symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))))

(define sample-plaintext '(a d a b b c a))
(encode sample-plaintext sample-tree)

;;;;;;;;;
;;2.69
;;;;;;;;;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (successive-merge leaf-set)
  (if (null? leaf-set)
      (display "Error - leaf-set empty")
      (let ((first (car leaf-set))
	    (rest (cdr leaf-set)))
	(if (null? rest)
	    first
	    (successive-merge (adjoin-set (make-code-tree first (car rest))
					  (cdr rest)))))))

;;;;;;;;;
;;2.70
;;;;;;;;;

(define alphabet '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3)
		   (yip 9) (wah 1)))

(define huffman-tree (generate-huffman-tree alphabet))

(define message '(get a job sha na na na na na na na na get a job sha
		      na na na na na na na na wah yip yip yip yip
		      yip yip yip yip yip sha boom))

(length (encode message huffman-tree))

;; 84 bits are required to encode this message

(* 3 (length message))

;; 108 bits is the smallest number of bits needed to encode this song
;; if we used a fixed-length code (3 bits per symbol, since we have 8
;; symbols and log_2 8 = 3.

;;;;;;;;;
;;2.71
;;;;;;;;;

;; For n=5, let our symbol-frequency pairs be
;; ((a 1) (b 2) (c 4) (d 8) (e 16))

;; Our Huffman tree then looks like:
;; 
;;     (a b c d e)
;;         /\
;;        /  \
;;       e   /\
;;          /  \
;;         d   /\
;;            /  \
;;           c   /\
;;              /  \
;;             b    a 
;; 
;; In general, the Huffman tree will look like this, since 
;; 2^n + 2^(n-1) < 2^(n+2) for n>=0.
;; 
;; The most frequent symbol will always just need one bit (only need
;; to go down one branch to get to it), whereas the least frequent
;; symbol will need n-1 bits, as we need to traverse the height of the
;; tree to get to it (the height of this tree is n-1).

;;;;;;;;;
;;2.72
;;;;;;;;;

;; Let n be the number of symbols in the alphabet of 2.71.
;; It would take O(n) time to encode the most frequent symbol
;; (O(n) to search the symbol list since all we're doing is a linear
;; search, and O(1) to actually produce the encoding since it's only
;; one cons operation).
;; 
;; To encode the least frequent symbol, it would take O(n^2) time,
;; since the symbol list decreases by 1 at each level, and so by the
;; time we've reached the actual symbol, our search time is 
;; (n-1)+(n-2)+...+1 = O(n^2). We would have n-1 cons operations in the
;; end, but the n^2 term dominates, so O(n^2) time.

;;;;;;;;;
;;2.73
;;;;;;;;;

;; a.

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp)) (operands exp) var))))

;; We are deciding what rule to apply by looking up the derivation
;; rule in our function table. The correct derivation rule is returned
;; depending on what symbol the arithmetic operator is (e.g. the rule
;; for a multiplication would be retrieved by (get 'deriv '*).
;; 
;; number? and variable? can't be assimilated into this data-directed
;; dispatch because they don't have associated operator symbols.

;; b.

(define (install-deriv-sum)
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
	      (deriv (cadr operands) var)))
  (put 'deriv '+ deriv-sum))

(define (install-deriv-product)
  (define (deriv-product operands var)
    (make-sum
     (make-product (car operands)
		   (deriv (cadr operands) var))
     (make-product (deriv (car operands) var)
		   (cadr operands))))
  (put 'deriv '* deriv-product))

;; c.

(define (install-deriv-exponentiation)
  (define (deriv-exponentiation operands var)
    (make-product (exponent exp)
		  (make-product (make-exponentiation (base exp)
						     (- (exponent exp)
							1))
				(deriv (base exp) var))))
  (put 'deriv '** deriv-exponentiation))

;; d.

;; We would need to change how we call put. e.g:
;; 
;; (put 'deriv '* deriv-product) 
;; 
;; becomes
;; 
;; (put '* 'deriv deriv-product)

;;;;;;;;;
;;2.74
;;;;;;;;;

;; a.

;; Add a division name as a type-tag to each file. Each file's type
;; would be its division name.

(define (get-record employee file)
  ((get 'get-record (division-name file)) employee (contents file)))

;; b.

;; Since each record is structured differently from division to
;; division, we need to call the correct salary selector.

(define (get-salary employee-record)
  (apply-generic 'get-salary employee-record))

;; c.

(define (find-employee-record employee files)
  (if (null? files)
      #f
      (let ((record (get-record employee (car files))))
	(if record
	    record
	    (find-employee-record employee (cdr files))))))

;; d.

;; Each division's personnel file should be tagged with the division's
;; name. Also, some sort of (install-new-division division) function
;; should be defined (one that puts the correct functions in the
;; dispatch table, so that they can be retrieved with get when
;; needed).

;;;;;;;;;
;;2.75
;;;;;;;;;

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) a)
	  (else
	   (display "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;;;;;;;;
;;2.76
;;;;;;;;;

;; 1) Generic operations with explicit dispatch.
;; 2) Data-directed dispatch.
;; 3) Dispatch through message-passing.
;; 
;; For adding new types, (1) would require that every generic
;; operation be modified to check for the new type, (2) would require
;; that the new type install its package of operations in the global
;; dispatch table before it can be used, (3) would simply require that
;; the new type's constructor define and return the appropriate dispatch
;; function - no change to the rest of the system.
;; 
;; For adding new operations, (1) requires that the new operation
;; implement a whole bunch of case statements to check for each type that
;; supports this operation, (2) requires that each type that supports
;; this operation register this support in the global dispatch table,
;; (3) requires that each type which supports this operation registers
;; its support in the dispatch function returned by its constructor.
;; 
;; In terms of pain, (1) is the most painful so it's out of the
;; picture. For adding new types, (3) seems the most suitable. For
;; adding new operations, (2) and (3) seem equally suitable, since in either 
;; case you're adding one line.

;;;;;;;;;
;;2.77
;;;;;;;;;

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; Consider the representation of 3+4i in rectangular form:
;; 
;; ('complex 'rectangular (3 4))
;; 
;; Recall that the procedure magnitude has been defined as 
;; 
;; (define (magnitude z) (apply-generic 'magnitude z))
;; 
;; thus tracing through the call (magnitude z), we see that it calls
;; (apply-generic 'magnitude z), which strips off the type-tag of z,
;; which is 'complex at this point, and then applies the procedure
;; returned by (get 'magnitude 'complex). This procedure turns out to
;; be magnitude, as decided by the put statement above. Thus magnitude
;; is called on the remaining contents of z, which are 
;; 
;; ('rectangular (3 4))
;; 
;; This causes a call of (apply-generic 'magnitude z), and again the
;; type-tag (this time it's 'rectangular) is stripped off, and the
;; procedure returned by (get 'magnitude 'rectangular) is called on (3 4).
;; 
;; Thus we see that apply-generic is invoked twice.

;;;;;;;;;
;;2.78
;;;;;;;;;

;; Old definitions:

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; New definitions:

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE_TAG" datum))))

(define (contents datum)
  (if (number? datum)
      contents
      (if (pair? datum)
	  (cdr datum)
	  (error "Bad tagged datum -- CONTENTS" datum))))

;;;;;;;;;
;;2.79
;;;;;;;;;

(define (equ? a b)
  (apply-generic 'equ? a b))

;; extra stuff in (install-scheme-number-package):

(put 'equ? '(scheme-number scheme-number) =)

;; extra stuff in (install-rational-package):

(define (equ? a b)
  (and (= (numer a) (numer b))
       (= (denom a) (denom b))))

(put 'equ? '(rational rational) equ?)

;; extra stuff in (install-complex-package):

(define (equ? a b)
  (and (= (real-part a) (real-part b))
       (= (imag-part a) (imag-part b))))

(put 'equ? '(complex complex) equ?)

;;;;;;;;;
;;2.80
;;;;;;;;;
     
(define (=zero? a)
  (apply-generic '=zero? a))

;; extra stuff in (install-scheme-number-package)

(define (=zero? a)
  (= a 0))

(put '=zero? 'scheme-number =zero?)

;; extra stuff in (install-rational-package)

(define (=zero? a)
  (= (numer a) 0))

(put '=zero? 'rational =zero?)

;; extra stuff in (install-complex-package)

(define (=zero? z)
  (= (magnitude z) 0))

(put '=zero? 'complex =zero?)

;;;;;;;;;
;;2.81
;;;;;;;;;

;; a.

;; If apply-generic is called with two arguments of type scheme-number
;; or complex for an operation that is not found in the table for
;; those types, then the type-conversion functions
;; scheme-number->scheme-number and complex->complex and invoked,
;; followed by a recursive call to apply-generic. However, note that
;; all these two conversions do is return the same type, thus there is
;; never any change, and so we will be trapped in infinite recursion.
;; 
;; After adding 'exp to Scheme-number package, we still end up with
;; infinite recursion when calling exp with two complex numbers as
;; arguments (since no version of exp exists for two complex
;; arguments).

;; b.

;; No, Louis is not correct. apply-generic would work as is, as long
;; as T->T conversions are not defined for some type T. This would
;; mean that when apply-generic is called with two arguments of the
;; same type for an operation not found in the table, it would end up
;; in the error cause of the internal cond, since no T->T conversions exist.

;; c.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (not (eq? type1 type2))
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))
		    (error "No method for these types"
			   (list op type-tags)))
		(error "No method for these types"
		       (list op type-tags))))))))

;;;;;;;;;
;;2.82
;;;;;;;;;

(define (and-list pred mylist)
  (if (null? mylist)
      #t
      (and (pred (car mylist)) (and-list pred (cdr mylist)))))

(define (all-eq? typelist)
  (let ((typecomp (car typelist)))
    (and-list (lambda (x) (eq? x typecomp)) (cdr typelist))))

(define (remove item sequence)
  (filter (lambda (x) (not (eq? x item))) sequence))

(define (coerce-args args coerc-funcs)
  (map (lambda (arg coercion) (coercion (contents arg)) (args coerc-funcs))))

(define (make-new-args args type-tags)
  (define (get-coercions type-tags tags-to-process)
    (if (null? tags-to-process)
	(error "not all types are convertable" type-tags)
	(let ((target-tag (car tags-to-process)))
	  (let ((current-argtypes (map (lambda (x) target-tag) type-tags))
		(coerce-funcs (map (lambda (tag) (if (eq? tag target-tag)
						     (lambda (x) tag)
						     (get-coercion tag
								   target-tag)))
				   type-tags)))
	    (if (and (get op current-argtypes)
		     (and-list (lambda (x) (not (null? x))) coerce-funcs))
		coerce-funcs
		(get-coercions type-tags (cdr tags-to-process)))))))
    (let ((coercions (get-coercions type-tags type-tags)))
      (coerce-args args coercions)))    

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (not (all-eq? type-tags))
	      (let ((new-args (make-new-args args type-tags)))
		(if (not (null? new-args))
		    (apply proc (map contents newargs))
		    (error "no method for these types"
			   (list op type-tags)))
		(error "no method for these types"
		       (list op type-tags)))
	      (error "no method for these types"
		     (list op type-tags)))))))

;; This is not general enough. Consider the case where we have the
;; following typelist:
;; 
;; (a b c)
;; 
;; where the following conversion methods exist:
;; 
;; b->a
;; c->b
;; a->c
;; 
;; The method will start with a, and will try to convert b to a
;; (success), and then c to a (failure). It will then move onto
;; b. This will fail because a is not convertible to b
;; directly. Finally it will move to c. But this will also fail
;; because b is not directly convertible to c.
;; 
;; Note that this problem can be sorted out if a proper hierarchy of
;; types is known to the system and can be investigated before
;; deciding on whether a type conversion is possible or not.

;;;;;;;;;
;;2.83
;;;;;;;;;

(define (install-raise-package)
  (define (raise-integer->rational n)
    (make-rational n 1))
  (define (raise-rational->real a)
    (/ (numer a) (denom a)))
  (define (raise-real->complex a)
    ((get 'make-from-real-imag 'complex) (a 0)))
  (define (raise-complex z) z)

  (put 'raise 'scheme-number raise-integer->rational)
  (put 'raise 'rational raise-rational->real)
  (put 'raise 'real raise-real->complex))

(define (raise number)
  (let ((type (type-tag number)))
    (let ((proc (get 'raise type)))
      (if proc
	  (proc number)
	  (error "No raise operation for this type" type)))))

;;;;;;;;;
;;2.84
;;;;;;;;;

(define (elem-index elem mylist)
  (define (position counter mylist)
    (cond ((null? mylist) #f)
	  ((eq? (car mylist) elem) counter)
	  (else (position (+ counter 1) (cdr mylist)))))
  (position 0 mylist))

(define (highest-type types tower)
  (define (higher type1 type2)
    (let ((t1-index (elem-index type1 tower))
	  (t2-index (elem-index type2 tower)))
      (if (< t1-index t2-index)
	  type2
	  type1)))
  (define (highest-internal types current-type)
    (if (null? types)
	current-type
	(let ((current-highest (higher (car types) current-type)))
	  (highest-internal (cdr types) current-highest))))
  (highest-internal (cdr types) (car types)))

(define (raise-all args target)
  (define (raise->target arg)
    (let ((newarg (raise arg)))
      (if (eq? (type newarg) target)
	  newarg
	  (raise->target newarg))))
  (map raise->target args))

;; system-wide tower

(define tower '(scheme-number rational real complex))
    
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (not (all-eq? type-tags))
	      (let ((target-type (highest-type type-tags tower)))
		(let ((newargs (raise-all args target-type)))
		  (let ((proc (get op (map (lambda (x) target-type)
					   type-tags))))
		    (if proc
			(apply proc (map contents newargs))
			(error "no method for these types"
			   (list op type-tags))))))
	      (error "no method for these types"
		     (list op type-tags)))
	  (error "no method for these types"
		 (list op type-tags))))))

;;;;;;;;;
;;2.85
;;;;;;;;;

(define (install-project-package)
  (define (project-complex->real z)
    (attach-tag 'real (real-part z)))
  (define (project-real->rational a)
    (make-rat (round a) 1))
  (define (project-rational->integer a)
    (attach-tag 'scheme-number (round (/ (numer a) (denom a)))))

  (put 'project 'complex project-complex->real)
  (put 'project 'real project-real->rational)
  (put 'project 'rational project-rational->integer))

(define (project number)
  (let ((type ((type-tag number))))
    (let ((proc (get 'project type)))
      (if proc
	  (proc number)
	  (error "No projection method for this type" type)))))

(define (droppable? arg)
  (let ((result (project arg)))
    (if result
	(equ? (contents arg) (contents (raise result)))
	#f)))

(define (drop arg)
  (if (droppable? arg)
      (drop (project arg))
      arg))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (not (all-eq? type-tags))
	      (let ((target-type (highest-type type-tags tower)))
		(let ((newargs (raise-all args target-type)))
		  (let ((proc (get op (map (lambda (x) target-type)
					   type-tags))))
		    (if proc
			(let ((result (apply proc (map contents newargs))))
			  (map drop result))
			(error "no method for these types"
			   (list op type-tags))))))
	      (error "no method for these types"
		     (list op type-tags)))
	  (error "no method for these types"
		 (list op type-tags))))))

;;;;;;;;;
;;2.86
;;;;;;;;;

;; We'd have to modify the complex arithmetic operations to use
;; apply-generic, which would raise/lower types accordingly, e.g.:

;; internal procedures in INSTALL-COMPLEX-PACKAGE

(define (add-complex z1 z2)
  (make-from-real-imag (apply-generic 'add (real-part z1)
				      (real-part z2))
		       (apply-generic 'add (imag-part z1)
				      (image-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (apply-generic 'sub (real-part z1)
				      (real-part z2))
		       (apply-generic 'sub (imag-part z1)
				      (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (apply-generic 'mul (magnitude z1)
				    (magnitude z2))
		     (apply-generic 'add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (apply-generic 'div (magnitude z1)
				    (magnitude z2))
		     (apply-generic 'sub (angle z1) (angle z2))))

;; this solution is incomplete, this part of the book is very tedious
;; (but final version apply-generic is pretty sweet I suppose)

;;;;;;;;;
;;2.87
;;;;;;;;;

(define (=zero-poly? poly)
  (define (zero-poly-iter result terms)
    (cond ((null? terms) result)
	  ((eq? #f result) result)
	  (else (zero-poly-iter (and result
				     (=zero? (coeff (first-term
						     terms))))
				(rest-terms terms)))))
  (zero-poly-iter #t (term-list poly)))

;; add to INSTALL-POLYNOMIAL-PACKAGE
(put '=zero? 'polynomial =zero-poly?)

(define terms '((100 0) (2 1) (0 0)))
(define poly (cons 'x terms))
(define (term-list poly) (cdr poly))
(define (coeff term) (cadr term))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define term '(100 0))
(=zero-poly? poly)

;;;;;;;;;
;;2.88
;;;;;;;;;

;; define our generic negation operations

(define (negate-int n)
  (if (= n 0)
      0
      (- 0 n)))

;; in INSTALL-SCHEME-NUMBER-PACKAGE:
(put 'negate '(scheme-number)
     (lambda (x) (tag (negate-int x))))

;; in INSTALL-RATIONAL-PACKAGE:
(put 'negate '(rational-number)
     (lambda (x) (tag (make-rational (negate-int (numer x))
				     (denom x)))))

;; in INSTALL-COMPLEX-PACKAGE
(put 'negate '(complex)
     (lambda (z) (tag (make-from-real-imag (negate (real-part z))
					   (negate (imag-part z))))))

;; in INSTALL-POLYNOMIAL-PACKAGE
(define (negate-poly poly)
  (if (empty-termlist? (term-list poly))
      (the-empty-termlist)
      (make-poly (variable poly) (map (lambda (term)
					(make-term (order term)
						   ((get 'negate
							 (type-tag
							  (coeff term)))
						    (coeff term))))
				      (term-list poly)))))

(define (sub-poly p1 p2)
  (add-poly p1 (negate-poly p2)))

(put 'negate '(polynomial)
     (lambda (poly) (tag (negate-poly poly))))

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))

(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable poly) (car poly))
(define (order term) (car term))
(define (make-term order coeff) (list order coeff))
(variable poly)
(negate-poly poly)
(negate 0)
poly

;;;;;;;;;
;;2.89
;;;;;;;;;

;; in this alternative representation, we look at termlists and
;; individual terms as the same thing, because we don't have a list
;; representation where each coefficient is paired with the term's
;; order. These are the necessary procedures to redefine:

(define (make-term order coeff)
  (define (make-term-iter count result)
    (if (= count order)
	(cons coeff result)
	(make-term-iter (+ count 1) (cons 0 result))))
  (make-term-iter 0 '()))

(define (coeff term)
  (car term))

(define (order term)
  (- (length term) 1))

(define (adjoin term termlist)
  (if (=zero? (coeff term))
      termlist
      (cons (car term) termlist)))

;; first-term is a no-op
(define (first-term term) term)

;;;;;;;;;
;;2.90
;;;;;;;;;

;; Not done. Exercises in this section are so tedious.
;; 
;; Just have type tags that would distinguish between the two
;; representations, then make the operations generic and have them
;; dispatch based on type tag, as was done for complex numbers.
;;
;; Have an (install-polynomial-package) that would sort it out.

;;;;;;;;;
;;2.91
;;;;;;;;;

;; assume we have procedure named sub-terms, analogous to add-terms as
;; defined in text.

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
	    (t2 (first-term L2)))
	(if (< (order t1) (order t2))
	    (list (the-empty-termlist) L1)
	    (let ((new-c (div (coeff t1) (coeff t2)))
		  (new-o (- (order t1) (order t2))))
	      (let ((rest-of-result
		     (div-terms (sub-terms L1 (mul-term-by-all-terms
					       (make-term new-o new-c) L2))
				L2)))
		(list (adjoin-term (make-term new-o new-c)
				   (car rest-of-result))
		      (cadr result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map (lambda (termlist) (make-poly (variable p1) termlist))
	   (div-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in same variable -- DIV-POLY"
	     (list p1 p2))))
		 
;;;;;;;;;
;;2.92
;;;;;;;;;

;; not done

;;;;;;;;;
;;2.93
;;;;;;;;;

;; Everything for this is already done, only thing is to change
;; make-rat so that it does not attempt to reduce fractions to lowest
;; terms:

(define (make-rat n d) (cons n d))

;;;;;;;;;
;;2.94
;;;;;;;;;

(define (remainder-terms L1 L2)
  (cadr (div-terms L1 L2)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms a (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (gcd-terms (term-list p1)
						(term-list p2)))
      (error "Polynomials not in same variable -- GCD-POLY"
	     (list p1 p2))))

(define (gcd-integer a b)
  (if (= b 0)
      a
      (gcd-integer b (remainder a b))))

(put 'gcd '(polynomial polynomial) gcd-poly)
(put 'gcd '(scheme-number scheme-number) gcd-integer)

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))

;;;;;;;;;
;;2.95
;;;;;;;;;

;; Not done, but here (Scheme48), division of integers returns
;; rationals.

;;;;;;;;;
;;2.96
;;;;;;;;;

;; a.

(define (pseudoremainder-terms L1 L2)
  (let ((integ-factor (exp (coeff (first-term L2)) (add 1
							(sub (order L1)
							     (order L2))))))
    (cadr (div-terms (mul-term-by-all-terms (make-term 0 integ-factor) L1)
		     L2))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms a (pseudoremainder-terms a b))))

;; b.

(define (gcd-terms a b)
  (define (gcd-terms-internal a b)
    (if (empty-termlist? b)
	a
	(gcd-terms-internal a (pseudoremainder-terms a b))))
  (let ((termsgcd (gcd-terms-internal a b)))
    (let ((gcd-coeffs (gcd (map coeffs termsgcd))))
      (map (lambda (term) (make-term (order term) (/ (coeff term)
						     gcd-coeffs)))
	   termsgcd))))

;;;;;;;;;
;;2.97
;;;;;;;;;

;; a.

(define (reduce-terms n d)
  (let ((gcdterms (gcd-terms a b)))
    (let ((integ-factor (exp (coeff (first term gcdterms))
			     (add 1 (sub (max (order (first-term n))
					      (order (first-term d)))
					 (order (first-term
						 gcdterms)))))))
      (let ((new-n (div-terms (mul-term-by-all-terms integ-factor n)
			      gcdterms))
	    (new-d (div-terms (mul-term-by-all-terms integ-factor d)
			      gcdterms)))
	(let ((gcdcoeffs-n (gcd (map coeffs new-n)))
	      (gcdcoeffs-d (gcd (map coeffs new-d))))
	  (let ((gcdcoeffs (gcd (gcdcoeffs-n gcdcoeffs-d))))
	    (list (map (lambda (term) (make-term (order term)
						 (/ (coeff term)
						    gcdcoeffs)))
		       new-n)
		  (map (lambda (term) (make-term (order term)
						 (/ (coeff term)
						    gcdcoeffs)))
		       new-d))))))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (map (lambda (termlist) (make-poly (variable p1) termlist))
	   (reduce-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in same variable -- REDUCE-POLY"
	     (list p1 p2))))

;; b.

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(polynomial polynomial) reduce-poly)
(put 'reduce '(scheme-number scheme-number) reduce-integers)

(define (reduce n d)
  (apply-generic 'reduce n d))

;; Note that (make-rat) as defined previously returns a cons pair.
(define (make-rat n d)
  (let ((reduction (reduce n d)))
    (cons (car reduction) (cadr reduction))))

