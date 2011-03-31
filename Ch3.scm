;;;;;;;;;
;;3.1
;;;;;;;;;

(define (make-accumulator n)
  (lambda (c)
    (begin (set! n (+ n c))
	   n)))

;;;;;;;;;
;;3.2
;;;;;;;;;

(define (make-monitored f)
  (let ((numtimes 0))
    (define (how-many-calls?) numtimes)
    (define (reset-count) (set! numtimes 0))
    (define (mf arg)
      (cond ((eq? arg 'how-many-calls?) (how-many-calls?))
	    ((eq? arg 'reset-count) (reset-count))
	    (else (set! numtimes (+ numtimes 1))
		  (f arg))))
    mf))
  
;;;;;;;;;
;;3.3
;;;;;;;;;

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrong-password arg) "Incorrect password")
  (define (dispatch passwd proc)
    (if (eq? passwd password)
	(cond ((eq? proc 'withdraw) withdraw)
	      ((eq? proc 'deposit) deposit)
	      (else (error "Uknown request - MAKE-ACCOUNT"
			   proc)))
	wrong-password))
  dispatch)

;;;;;;;;;
;;3.4
;;;;;;;;;

(define (make-account balance password)
  (let ((num-wrong 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (wrong-password arg)
      (set! num-wrong (+ num-wrong 1))
      "Incorrect password")
    (define (call-the-cops arg) "Calling the cops!")
    (define (dispatch passwd m)
      (if (eq? passwd password)
	  (begin
	    (set! num-wrong 0)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Uknown request - MAKE-ACCOUNT"
			       m))))
	  (if (>= num-wrong 7)
	      call-the-cops
	      wrong-password)))
    dispatch))

;;;;;;;;;
;;3.5
;;;;;;;;;

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
	   
;; Using Scheme48. You have to ,use-package random in order to use
;; this.

;; Let 5 be our seed.
(define random (make-random 5))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (remainder (random) range))))

(define (estimate-integral P x1 y1 x2 y2 num-trials)
  (define (make-point x y) (cons x y))
  (define (experiment) (P (make-point (random-in-range x1 x2)
				      (random-in-range y1 y2))))
  (let ((rectangle-area (* (- x2 x1) (- y2 y1))))
    (* rectangle-area (monte-carlo num-trials experiment))))

(define (in-unit-circle? point)
  (let ((x (car point))
	(y (cdr point)))
    (<= (+ (expt (- x 5) 2) (expt (- y 7) 2)) 1)))

(/ (estimate-integral in-unit-circle? 4 6 6 8 10000) 1.0)

;;;;;;;;;
;;3.6
;;;;;;;;;

(define rand
  (let ((random (make-random 1)))
    (define (reset num) (set! random (make-random num)))
    (define (generate) (random))
    (define (dispatch arg)
      (cond ((eq? arg 'reset) reset)
	    ((eq? arg 'generate) (generate))))
    dispatch))

;;;;;;;;;
;;3.7
;;;;;;;;;

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (wrong-password arg) "Incorrect password")
  (define (check-password passwd) (eq? passwd password))
  (define (dispatch passwd proc)
    (if (eq? passwd password)
	(cond ((eq? proc 'withdraw) withdraw)
	      ((eq? proc 'deposit) deposit)
	      ((eq? proc 'check-password) check-password)
	      (else (error "Uknown request - MAKE-ACCOUNT"
			   proc)))
	wrong-password))
  dispatch)

(define (make-joint account oldpassword newpassword)
  (define (dispatch passwd proc)
    (define (wrong-password arg) "Incorrect password")
    (if (eq? passwd newpassword)
	(account oldpassword proc)
	wrong-password))
  (if ((account oldpassword 'check-password) oldpassword)
      dispatch
      "Incorrect password"))

(define peter-acc (make-account 50 'hello))
((peter-acc 'hello 'withdraw) 2)
(define paul-cc (make-joint peter-acc 'hello 'hi))
((paul-cc 'hi 'withdraw) 2)

;;;;;;;;;
;;3.8
;;;;;;;;;

(define f
  (let ((a 5))
    (define (f-internal n)
      (cond ((= n 0) (set! a 0)))
      a)
    f-internal))

;;;;;;;;;
;;3.9
;;;;;;;;;

;; see 3-9-recurse.png and 3-9-iter.png, respectively.

;;;;;;;;;
;;3.10
;;;;;;;;;

;; Done on paper. 
;; 
;; Both versions create objects with the same behaviour because each
;; call to (make-withdraw) creates a seperate environment where
;; balance is defined.
;; 
;; This is different from the environment structure of the book's
;; example because here we have an extra level of indirection (due to
;; the extra let - which is equivalent to lambda), thus we have an
;; extra environment.

;;;;;;;;;
;;3.11
;;;;;;;;;

;; acc is defined in the global environment, its local state is kept
;; in its own environment. If we define another account acc2, its
;; local state will be kept in a separate environment, thus both local
;; states of the two accounts are kept distinct. The one part of the
;; environment structure they both share is that the definition of
;; both acc and acc2 is the same (the definition of (dispatch)).

;;;;;;;;;
;;3.12
;;;;;;;;;

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(cdr x)
;; (b)

(define w (append! x y))

(cdr x)
;; (b c d)

;;;;;;;;;
;;3.13
;;;;;;;;;

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

;; With the box-and-pointer diagram representing z, the last box which
;; used to represent the end of the sequence now points to the
;; beginning of z, i.e. 'a. So you basically have a cycle.
;; 
;; Thus trying to compute (last-pair z) will be fruitless, since this
;; process will never terminate (it is never the case that (null? (cdr
;; x)) for all pairs x in z.

;;;;;;;;;
;;3.14
;;;;;;;;;

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

;; This function reverses a list in place. Note that there is no
;; cons'ing.
;;
;; Suppose v is defined by
;; 
;; (define v (list 'a 'b 'c 'd))
;; 
;; These are the intermediate stages when running (mystery v):
;; 
;; (loop (a b c d) '())
;; 
;; temp = (b c d)
;; (set-cdr! x '())
;; x = (a)
;; 
;; 
;; (loop (b c d) (a))
;; 
;; temp = (c d)
;; (set-cdr! x (a))
;; x = (b a)
;; 
;;
;; (loop (c d) (b a))
;; 
;; temp = (d)
;; (set-cdr! x (b a))
;; x = (c b a)
;;
;; 
;; (loop (d) (c b a))
;; 
;; temp = '()
;; (set-cdr! x (c b a))
;; (x = (d c b a))
;;
;; 
;; (loop '() (d c b a))
;;
;; return (d c b a)

;; Note that after running this, v is now just (a). This is the result
;; of the first call to the function which does (set-cdr! v '()). Thus
;; only the head element of v remains. After the first call, only
;; copies are modified, so v does not change after this.

;;;;;;;;;
;;3.15
;;;;;;;;;

;; Done on paper.

;;;;;;;;;
;;3.16
;;;;;;;;;

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons 'e 'f))

(define v (cons x y))
(count-pairs v)
;; 3

(define w (list (cons x x)))
;; 4

;; w actually only has 3 pairs: x is one pair, (cons x x) forms
;; another pair, and (list (cons x x)) forms another pair (a list of
;; one element), the car of which points to (cons x x) and the cdr is
;; null:

;; (list (cons x x))
;; +---+---+
;; | . | / |
;; +---+---+
;;   |
;;   |
;;  \/
;; (cons x x)
;; +---+---+
;; | . | . |
;; +---+---+
;;   |   |
;;   |   |
;;  \/  \/
;;    x
;; +---+---+
;; | . | . |
;; +---+---+
;;   |    |
;;   |    |
;;  \/   \/
;; +---+ +---+
;; | a | | b |
;; +---+ +---+

(define x2 (cons x x))

(define u (cons x2 x2))
(count-pairs u)
;; 7

;; same diagram as above, except that instead of the cdr of our first
;; pair being null, it also points to the same pair the car points to.

(define cycle (make-cycle (list 'a 'b 'c)))

(count-pairs cycle)
;; never terminates

;;;;;;;;;
;;3.17
;;;;;;;;;

(define (member? x mylist)
  (cond ((null? mylist) #f)
	((eq? x (car mylist)) #t)
	(else (member? x (cdr mylist)))))

(define (new-count-pairs x)
  (let ((seen '()))
    (define (internal-count x)
      (cond ((not (pair? x)) 0)
	    ((member? x seen) 0)
	    (else
	     (set! seen (cons x seen))
	     (+ 1
		(internal-count (car x))
		(internal-count (cdr x))))))
    (internal-count x)))

;;;;;;;;;
;;3.18
;;;;;;;;;

(define (cycle? mylist)
  (let ((seen '()))
    (define (cycle-internal? x)
      (cond ((null? x) #f)
	    ((not (pair? x)) #t)
	    ((member? x seen) #t)
	    (else
	     (set! seen (cons x seen))
	     (and (cycle-internal? (car x))
		  (cycle-internal? (cdr x))))))
    (cycle-internal? mylist)))
    
;; similar approach to new-count-pairs above.

;;;;;;;;;
;;3.19
;;;;;;;;;

(define (cycle? mylist)
  (define (check-cycle x1 x2)
    (cond ((or (null? x1) (null? x2)) #f)
	  ((eq? x1 x2) #t)
	  (else (check-cycle (cdr x1) (cddr x2)))))
  (check-cycle mylist (cddr mylist)))

;; This uses the "pointer chasing" algorithm that commonly comes up in
;; job interviews ("How would you tell if there's a cycle in this
;; linked list?").

;;;;;;;;;
;;3.20
;;;;;;;;;

;; Done on paper (actually no, I'm lying).

;; For (define x (cons 1 2)), x is created in the global environment,
;; points to a new environment that contains x = 1, y = 2, with the
;; global environment as this new environment's parent.

;;;;;;;;;
;;3.21
;;;;;;;;;

;; The problem is the Lisp printer is just displaying the car and cdr
;; of our queue. Since the car of our queue (front-ptr) points to a list of
;; elements, that list is being displayed. But the cdr of our queue
;; (rear-ptr) points to the last element, and so that is then also
;; displayed again, after having been already displayed as part of the car.

;; Here's a function that sorts this out:

(define (print-queue queue)
  (display (front-ptr queue)))

;;;;;;;;;
;;3.22
;;;;;;;;;

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (front-pointer) front-ptr)
    (define (rear-pointer) rear-ptr)
    (define (set-front-pointer! item) (set! front-ptr item))
    (define (set-rear-pointer! item) (set! rear-ptr item))
    (define (empty-q?) (null? (front-pointer)))
    (define (front-q)
      (if (empty-q?)
	  (error "FRONT called with an empty queue" queue)
	  (car (front-pointer))))
    (define (insert-q! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-q?)
	       (set-front-pointer! new-pair)
	       (set-rear-pointer! new-pair)
	       (cons front-ptr rear-ptr))
	      (else
	       (set-cdr! (rear-pointer) new-pair)
	       (set-rear-pointer! new-pair)
	       (cons front-ptr rear-ptr)))))
    (define (delete-q!)
      (cond ((empty-q?)
	     (error "DELETE! called with an empty queue" queue))
	    (else
	     (set-front-pointer! (cdr (front-pointer)))
	     (cons front-ptr rear-ptr))))
    (define (print-q)
      (display (front-pointer)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-q?)
	    ((eq? m 'front-queue) front-q)
	    ((eq? m 'insert-queue!) insert-q!)
	    ((eq? m 'delete-queue!) delete-q!)
	    ((eq? m 'print-queue) print-q)
	    (else (error "undefined operation"))))
    dispatch))

(define (empty-queue? queue)
  ((queue 'empty-queue?)))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (front-queue queue)
  ((queue 'front-queue)))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (print-queue queue)
  ((queue 'print-queue)))

(define q (make-queue))

;;;;;;;;;
;;3.23
;;;;;;;;;

;; We need a doubly-linked list for this.

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (or (null? (front-ptr deque))
				 (null? (rear-ptr deque))))

(define (make-deque) (cons '() '()))

(define (make-node item)
  (cons '() (list item)))

(define (prev-node node)
  (car node))

(define (next-node node)
  (cddr node))

(define (contents node)
  (cadr node))

(define (set-next-node! node nextnode)
  (set-cdr! (cdr node) nextnode))

(define (set-prev-node! node prevnode)
  (set-car! node prevnode))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (contents (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (contents (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-node item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-next-node! new-pair (front-ptr deque))
	   (set-prev-node! (front-ptr deque) new-pair)
	   (set-front-ptr! deque new-pair)
	   deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "FRONT-DELETE! called with an empty deque" deque))
	(else
	 (set-front-ptr! deque (next-node (front-ptr deque)))
	 (if (empty-deque? deque)
	     (set-rear-ptr! deque (front-ptr deque))
	     (set-prev-node! (front-ptr deque) '()))
	 deque)))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-node item)))
    (cond ((empty-deque? deque)
	   (set-front-ptr! deque new-pair)
	   (set-rear-ptr! deque new-pair)
	   deque)
	  (else
	   (set-next-node! (rear-ptr deque) new-pair)
	   (set-prev-node! new-pair (rear-ptr deque))
	   (set-rear-ptr! deque new-pair)
	   deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
	 (error "REAR-DELETE! called with an empty deque" deque))
	(else
	 (set-rear-ptr! deque (prev-node (rear-ptr deque)))
	 (if (empty-deque? deque)
	     (set-front-ptr! deque (rear-ptr deque))
	     (set-next-node! (rear-ptr deque) '()))
	 deque)))

;; testing testing 1,2,3
(define d (make-deque))
(front-insert-deque! d 'b)
(front-insert-deque! d 'a)
(front-delete-deque! d)
(rear-insert-deque! d 'c)
(rear-deque d)
(front-deque d)
(rear-insert-deque! d 'd)
(rear-delete-deque! d)

;;;;;;;;;
;;3.24
;;;;;;;;;

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records eqfunc)
      (cond ((null? records) #f)
	    ((eqfunc key (caar records)) (car records))
	    (else (assoc key (cdr records) eqfunc))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable) same-key?)))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable) same-key?)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Uknown operation -- TABLE" m))))
    dispatch))

;;;;;;;;;
;;3.25
;;;;;;;;;

;; Is it cheating to just look at this as a 1-dimensional table, where
;; each individual key is a list? Oh well.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((equal? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
	(if record
	    (cdr record)
	    #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! local-table
		      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Uknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;
;;3.26
;;;;;;;;;

;; Have a balanced BST. *table* would be a dummy node that points to
;; the root of the tree. So the whole table can be a (cons *table*
;; binary tree). From then on it's just a binary search to
;; find the wanted key (making sure to rebalance on insertion).

;;;;;;;;;
;;3.27
;;;;;;;;;

;; The number of steps is proportional to n because each Fib(k)
;; integer for some k is computed exactly once, since it is stored in
;; the table the first time it is computed, and simply retrieved all
;; subsequent times it is needed.
;;
;; memo-fib executes in the environment set up by the let in memoize,
;; where we have a frame that contains the definition for table.
;;
;; The scheme would not work had we defined memo-fib to be (memoize
;; fib) because then the recursive call would be going to fib, which
;; does not make use of any memoisation. We would still be calculating
;; duplicate values (i.e. we might as well just call fib directly).
;; The recursive call must memoise for our scheme to work.

;;;;;;;;;
;;3.28
;;;;;;;;;

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action a1 or-action-procedure)
  (add-action a2 or-action-procedure)
  'ok)

(define (logical-or c1 c2)
  (if (or (= c1 1) (= c2 1))
      1
      0))

;;;;;;;;;
;;3.29
;;;;;;;;;

(define (or-gate a b)
  (let ((c (make-wire))
	(d (make-wire))
	(e (make-wire))
	(s (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e s)))

;; De Morgan's laws make this easy: recall that p v q = ~(~p ^ ~q).
;;
;; The total delay time is 2*inverter-delay-time + and-gate-delay
;; (realise that the first two inverters run in parallel).

;;;;;;;;;
;;3.30
;;;;;;;;;

(define (ripple-carry-adder Alist Blist Slist C)
  (let ((carrytemp (make-wire)))
    (cond ((null? (cdr Alist))
	   (begin
	     (set-signal! carrytemp 0)
	     (full-adder (car Alist) (car Blist) carrytemp (car Slist) C)))
	  (else
	   (full-adder (car Alist) (car Blist) carrytemp (car Slist)
		       C)
	   (ripple-carry-adder (cdr Alist) (cdr Blist) (cdr Slist)
			       carrytemp)))))

;;;;;;;;;
;;3.31
;;;;;;;;;

;; We need the signal to propagate forwards. Consider the
;; half-adder. It has an and-gate connected to an inverter connected
;; to an and. So, without initialising, we would have the output of
;; and1 be 0, thus input of inverter be 0, and also the output of
;; inverter be 0 (since that same wire is the input of and2). This is
;; obviously false. However, if we have the initalisation step, then
;; the output of the inverter (and thus the input of and2) will be 1,
;; which is correct.

;;;;;;;;;
;;3.32
;;;;;;;;;

;; Suppose we construct an and-gate (with inputs 0 and 0). Then, we
;; set-signal of input wire A to 1, and input wire B to 1. Thus, when
;; running the procedures in our agenda, the final output of the
;; and-gate should be 1. However, if they are run in LIFO
;; (last-in-first-out) order, then the final output is 0, which is
;; obviously incorrect. Thus, we need a queue, as the whole intention
;; behind this is to run various operations in order.

;;;;;;;;;
;;3.33
;;;;;;;;;

;; Obviously, average = arithmetic mean here, i.e. (a+b)/2 = c. So,
;; our constraint is 2*c = a+b

(define error display)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
	   (set-value! sum
		       (+ (get-value a1) (get-value a2))
		       me))
	  ((and (has-value? a1) (has-value? sum))
	   (set-value! a2
		       (- (get-value sum) (get-value a1))
		       me))
	  ((and (has-value? a2) (has-value? sum))
	   (set-value! a1
		       (- (get-value sum) (get-value a2))
		       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (display "Uknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
	       (and (has-value? m2) (= (get-value m2) 0)))
	   (set-value! product 0 me))
	  ((and (has-value? m1) (has-value? m2))
	   (set-value! product
		       (* (get-value m1) (get-value m2))
		       me))
	  ((and (has-value? product) (has-value? m1))
	   (set-value! m2
		       (/ (get-value product) (get-value m1))
		       me))
	  ((and (has-value? product) (has-value? m2))
	   (set-value! m1
		       (/ (get-value product) (get-value m2))
		       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (display "Uknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (display "Uknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (display "Uknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
	     (set! value newval)
	     (set! informant setter)
	     (for-each-except setter
			      inform-about-value
			      constraints))
	    ((not (= value newval))
	     (display "Contradiction" (list value newval)))
	    (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
	  (begin (set! informant #f)
		 (for-each-except retractor
				  inform-about-no-value
				  constraints))
	  'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
	  (set! constraints
		(cons new-constraint constraints)))
      (if (has-value? me)
	  (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
	     (if informant #t #f))
	    ((eq? request 'value) value)
	    ((eq? request 'set-value!) set-my-value)
	    ((eq? request 'forget) forget-my-value)
	    ((eq? request 'connect) connect)
	    (else (display "Uknown operation -- CONNECTOR"
			 request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
	  ((eq? (car items) exception) (loop (cdr items)))
	  (else (procedure (car items))
		(loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


(define (averager a b c)
  (let ((two (make-connector))
	(u (make-connector)))
    (adder a b u)
    (multiplier c two u)
    (constant 2 two)))

;;;;;;;;;
;;3.34
;;;;;;;;;

;; The main problem with this is that it doesn't work as it seems. For
;; example, if a isn't known yet b is, we have enough information to
;; compute a (sqrt(b)), yet the system as set up won't do that, since
;; the multiplier requires two knowns, not just one.

;;;;;;;;;
;;3.35
;;;;;;;;;

(define (square x) (* x x))
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
	   (process-new-value))
	  ((eq? request 'I-lost-my-value)
	   (process-forget-value))
	  (else
	   (error "Uknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;;;;;;;;
;;3.36
;;;;;;;;;

;; Done on paper (not really).

;;;;;;;;;
;;3.37
;;;;;;;;;

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv c)
  (let ((z (make-connector)))
    (constant c z)
    z))

;;;;;;;;;
;;3.38
;;;;;;;;;

;; a.

;; Note that we have 3! = 6 permutations:
;; 
;; Peter->Paul->Mary: Balance = 45
;; Peter->Mary->Paul: Balance = 35
;; Mary->Peter->Paul: Balance = 40
;; Mary->Paul->Peter: Balance = 40
;; Paul->Mary->Peter: Balance = 50
;; Paul->Peter->Mary: Balance = 45

;; b.

;; Quite a few values possible here, one being balance = 50.

;;;;;;;;;
;;3.39
;;;;;;;;;

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
		  (s (lambda () (set! x (+ x 1)))))

;; 3 possibilities here:
;; 
;; 101: P1 followed by P2.
;; 121: P2 followed by P1.
;; 100: P1 accesses x (where x=10) with set!, then P2 runs, then P1
;; finishes up, with x*x = 10*10 = 100.

;;;;;;;;;
;;3.40
;;;;;;;;;

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
		  (lambda () (set! x (* x x x))))

;; Possible values are:
;; 
;; 1,000,000: P1 runs to completion, followed by P2.
;; 100: P1 access x twice, then P2 sets x to 1000, then P1 sets x.
;; 1000: P2 accesses x, then P1 sets x to 100, then P2 sets x.
;; 100,000: P1 changes x from 10 to 100 between the first two times
;;          that P2 accesses the value of x during the evaluation of
;;          (* x x x).
;; 10,000: P1 changes x from 10 to 100 between the last two times that
;;         P2 accesses the value of x during the evaluation of (* x x x).
;; 10,000: P2 changes x from 10 to 1000 between the two times that
;;         P1 accesses the value of x during the evaluation of (* x x).
;; 1,000,000: P2 runs to completion, followed by P1.

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
		  (s (lambda () (set! x (* x x x)))))

;; With the serialiser, the only two possible scenarios are P1
;; executes to completion followed by P2, or P2 executes to completion
;; followed by P1. Both cases yield 1,000,000.

;;;;;;;;;
;;3.41
;;;;;;;;;

;; Since all we're doing is reading the value and not modifying it, we
;; don't have to serialise it.

;;;;;;;;;
;;3.42
;;;;;;;;;

;; Seems fine to me - all we're doing is binding the evaluation to a
;; name and then returning that instead of returning the result of the
;; evaluation directly.

;;;;;;;;;
;;3.43
;;;;;;;;;

;; The exchange procedure can be thought of as a permutation of the
;; set {10, 20, 30}. If we have a set {a, b, c}, an exchange operation
;; f is defined by f(a) = b, f(b) = a, f(c) = c. We are not
;; introducing any new elements.
;; 
;; This condition can be violated if the exchanges are implemented
;; using the first version of the exchange procedure. For example,
;; let's say that Peter and Paul both have access to accounts a1, a2,
;; and a3 (a1 = 10, a2 = 20, a3 = 30), where Peter exchanges a1 and a2
;; while Paul concurrently exchanges a1 and a3. Peter might compute
;; the difference in the balances for a1 and a2, but then Paul might
;; change the balance in a1 to 30. Peter than continues with his
;; transaction, thinking the difference is -10, when in fact it is 10
;; because at this point we have a1 = 30, a2 = 20, a3 = 10. Thus after
;; Peter is done with his exchange, we have a1 = 40, a2 = 10, a3 = 10.
;; 
;; Even with this exchange program, the sum of the balances in the
;; accounts will be preserved, because the deposit and withdraw
;; procedures are serialised. Thus, whatever difference is calculated
;; from the two accounts, it is guaranteed that one account will be
;; decreased by this amount, while the other will be increased by it.
;; 
;; This condition would be violated if we did not serialise the
;; transactions on the individual accounts. Consider the same scenario
;; as above:
;; 
;; Peter and Paul both have access to accounts a1, a2, and a3 (a1 =
;; 10, a2 = 20, a3 = 30), where Peter exchanges a1 and a2 while Paul
;; concurrently exchanges a1 and a3. Peter computes the difference
;; between a1 and a2 to be -10, while Paul computes the difference
;; between a1 and 3 to be -20. When Peter access a1 to withdraw -10, and
;; before he commits, Paul access a1 to withdraw -20. Peter then sets
;; a1 to be 20, but Paul is not done with his transaction, and when he
;; is, a1 is actually set to be 30. Peter then deposits -10 in a2, and
;; Paul deposits -20 in a3, and our final balances are now
;; 
;; a1 = 30, a2 = 10, a3 = 10
;; 
;; This is obviously different from the total we started with (60).

;;;;;;;;;
;;3.44
;;;;;;;;;

;; Louis is wrong. The difference between the transfer procedure and
;; the exchange procedure is that the exchange procedure accesses the
;; account values (to calculate the difference) in a non-serialised
;; manner, and then it proceeds to the serialised sections. Thus it is
;; possible for accesses to interleave in the non-serialised region,
;; causing an incorrect calculation for difference, and thus incorrect
;; deposits and withdrawls.
;; 
;; With the transfer procedure, there are no calculations that depend
;; on information accessed in a non-serialised manner. In fact, both
;; the operations involved in transfer are serialised (contrast with
;; exchange, which has a third operation that is not serialised, as
;; mentioned above), so we are ok.

;;;;;;;;;
;;3.45
;;;;;;;;;

;; The problem with Louis's reasoning is that we end up using the same
;; serialiser a second time, while it is already in use. When
;; serialized-exchange is called, we have used the two serialisers
;; that belong to the accounts to serialise the whole exchange
;; procedure. But then internally, when withdraw and deposit are
;; called, an attempt is made to use the same serialisers, and so we
;; have a deadlock, where we wait to use the serialiser internally,
;; but we will never be able to because it has already been called for
;; the whole function (which we are stuck in).

;;;;;;;;;
;;3.46
;;;;;;;;;

;; Can't be bothered to draw, but here is a problematic scenario:
;; 
;; Suppose we have processes P1 and P2. P1 accesses the mutex's cell
;; in the call to (car cell), but then P2 starts and also accesses
;; (car cell), which hasn't been modified by P1 yet, so P2 continues
;; on and acquires the mutex, setting (car cell) to true. However, P1
;; has already accessed (car cell) before this modification, and so it
;; thinks that it is false (when in fact P2 has set it to be true),
;; and so P1 gets to acquire the mutex too.

;;;;;;;;;
;;3.47
;;;;;;;;;

;; a.

;; Basic idea: we have a counter set to n which we decrement whenever
;; we acquire, and increment whenever we release. If counter = 0, we
;; wait until something is released.

(define (make-semaphore n)
  (let ((acquire-access (make-mutex))
	(num-available n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (acquire-access 'acquire)
	     (if (= num-available 0)
		 (begin
		   (acquire-access 'release)
		   (the-semaphore 'acquire))
		 (begin
		   (set! num-available (- num-available 1))
		   (acquire-access 'release))))
	    ((eq? m 'release)
	     (set! num-available (+ num-available 1))
    the-semaphore)))))

;; b.

(define (test-and-set! n)
  (if (= n 0)
      #t
      (begin (set! n (- n 1))
	     #f)))

(define (make-semaphore n)
  (let ((num-available n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set-n! num-available)
		 (the-semaphore 'acquire))
	     ((eq? m 'release)
	      (set! num-available (+ num-available 1))))))
    the-semaphore))

;;;;;;;;;
;;3.48
;;;;;;;;;

;; The reason this works is that we are forcing the same order on
;; every process. Thus, for serialised resources R_n and R_m, m > n, it is
;; impossible that for two processes P1 and P2, P2 will have entered
;; R_m before R_n, while at the same time P1 is in R_n.

;; Order the accounts in increasing order
(define (ordered-accounts account1 account2)
  (if (< (account1 'account-id) (account2 'account-id))
      (cons account1 account2)
      (cons account2 account1)))

(define (serialized-exchange account1 account2)
  (let ((ordered-accts (ordered-accounts account1 account2)))
    (let ((serializer1 ((car ordered-accts) 'serializer))
	  (serializer2 ((cdr ordered-accts) 'serializer))
	  (acct1 (car ordered-accts))
	  (acct2 (cdr ordered-accts)))
      ((serializer1 (serializer2 exchange))
       acct1
       acct2))))

;; Modifying make-account
(define (make-account-and-serializer balance account-id)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'account-id) account-id)
	    (else (error "Uknown request -- MAKE-ACCOUNT" m))))))

delay

;;;;;;;;;
;;3.49
;;;;;;;;;

;;;;;;;;;
;;3.50
;;;;;;;;;

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

;;;;;;;;;
;;3.51
;;;;;;;;;

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0

(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5

(stream-ref x 7)
;; 6
;; 7

;; 0 to 5 are printed because of the call to DISPLAY-LINE from
;; SHOW. When we call (STREAM-REF x 7), only 6 and 7 are printed,
;; because of memoisation (all the other calls to show evaluated to x).
;; 
;; The crucial point to realise is that we have environments within
;; envrionments. For example, initially we have 
;; 
;; (CONS 0 (DELAY (STREAM-ENUMERATE-INTERVAL 1 10)))
;; 
;; where DELAY causes a new environment to be set-up (MEMO-PROC). When
;; we call CDR-STREAM on this, RESULT in this environment is now
;; 
;; (CONS 1 (DELAY (STREAM-ENUMERATE-INTERVAL 2 10)))
;; 
;; and we have another environment set up by this DELAY, inside of our
;; previous environment. 

;;;;;;;;;
;;3.52
;;;;;;;;;

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))
	       
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

;; sum = 1

(define y (stream-filter even? seq))
;; sum = 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))
;; sum = 10

(stream-ref y 7)
;; sum = 136 (since we start at 6 and keep adding, stopping at the 8th
;; (index 7) even number.
;; This procedure also returns 136.

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(display-stream z)
;; sum = 210, since we keep going until we get to the end of the stream.
;; This procedure returns 'done.

;; These reponses would indeed differ if we did not have any
;; memoisation provided by DELAY. For example, after defining z, sum
;; is at 10, which is also the point our stream is at. But when we
;; call STREAM-REF with y, we start back at 6, and so since 10 is
;; already generated, we don't accumulate sum for that value. With
;; memoisation, we make sure to only accumulate sum once per
;; value. Without it, we would accumulate every single time we
;; generate values from the stream, even if they have been generated
;; before.

;;;;;;;;;
;;3.53
;;;;;;;;;

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define s (cons-stream 1 (add-streams s s)))
;; This stream defines the sequence 2^n, n>=0.

;;;;;;;;;
;;3.54
;;;;;;;;;

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))
(stream-ref factorials 5)

;; In this case we have nth element is n!, which is contrary to what
;; the exercise asks for (nth element = (n+1)!). If you want to do it
;; as the book asks, start integers from 2. I prefer it my way.

;;;;;;;;;
;;3.55
;;;;;;;;;

(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

;;;;;;;;;
;;3.56
;;;;;;;;;

(define (scale-stream stream k)
  (stream-map (lambda (x) (* x k)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))


;;;;;;;;;
;;3.57
;;;;;;;;;

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))

;; n-1 additions are performed using this.

;;;;;;;;;
;;3.58
;;;;;;;;;

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)
;; 1 (expand 3 7 10)
;; 4 (expand 2 7 10)
;; 2 (expand 6 7 10)
;; 8 (expand 4 7 10)
;; 5 (expand 5 7 10)
;; 7 (expand 1 7 10)

(expand 3 8 10)
;; 3 (expand 6 8 10)
;; 7 (expand 4 8 10)
;; 5 (expand 0 8 10)
;; 0 (expand 0 8 10)

;; This is expanding num/den in base radix. e.g. 1/7 = 0.1428571...

;;;;;;;;;
;;3.59
;;;;;;;;;

;; a.

(define (integrate-series s)
  (stream-map / s integers))

;; b.

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;;;;;;;;
;;3.60
;;;;;;;;;

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2)
					  (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

;;;;;;;;;
;;3.61
;;;;;;;;;

(define (invert-unit-series S)
  (cons-stream 1 (stream-map -
		  (mul-series (stream-cdr S)
			      (invert-unit-series S)))))

;;;;;;;;;
;;3.62
;;;;;;;;;

(define (div-series s1 s2)
  (let ((s2-const-term (stream-car s2)))
    (if (= s2-const-term 0)
	(error "Denominator has a zero constant term -- DIV-SERIES")
	(mul-series s1 (scale-stream (invert-unit-series s2)
				     (/ s2-const-term))))))

(define tan-series (div-series sine-series cosine-series))

;; The reasoning behind this definition of div-series is identical to the
;; reasoning in the description of exercise 3.61, except we want to
;; generalise inversion for series with constant terms that might not
;; be 1. So, let S = c + S_R, and thus X = (1 - (S_R * X)) / c.

;;;;;;;;;
;;3.63
;;;;;;;;;

(define (sqrt-stream x)
  (cons-stream 1.0
	       (stream-map (lambda (guess)
			     (sqrt-improve guess x))
			   (sqrt-stream x))))

;; This version of the procedure is indeed less efficient since it
;; calls sqrt-stream every time, thus always constructs a new stream
;; (thus seperate environments when it comes to DELAY) in parallel to
;; the one it's already constructing. The original version on the
;; other hand only computes one stream, and re-uses the previous value
;; (through memoisation).
;;
;; If there was no memoisation by DELAY, then both versions would be
;; the same.

;;;;;;;;;
;;3.64
;;;;;;;;;

(define (stream-limit s tolerance)
  (let ((a1 (stream-ref s 0))
	(a2 (stream-ref s 1)))
    (if (< (abs (- a1 a2)) tolerance)
	a2
	(stream-limit (stream-cdr s) tolerance))))

;;;;;;;;;
;;3.65
;;;;;;;;;

(define (display-stream s n)
  (if (= n 0)
      'done
      (begin
	(display (stream-car s))
	(newline)
	(display-stream (stream-cdr s) (- n 1)))))

(define (square x) (* x x))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(display-guesses ln2-stream 8)
(display-guesses (euler-transform ln2-stream) 8)
(display-guesses (accelerated-sequence euler-transform ln2-stream) 8)

;; Of course, the last one converges most rapidly.

;;;;;;;;;
;;3.66
;;;;;;;;;

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define p (pairs integers integers))

(display-guesses p 100)

;; (1,100) is preceded by 197 pairs.
;; 
;; Quite difficult to provide a "closed form" for the pattern, but the
;; general idea is that we have two streams, the one which produces
;; lists of (1 n) and another one (which is the recursion), and we
;; keep alternating between the two. Thus every other pair produced
;; will be of the form (1 n).

;;;;;;;;;
;;3.67
;;;;;;;;;

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define p (pairs integers integers))

(display-guesses p 100)

;;;;;;;;;
;;3.68
;;;;;;;;;

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; This does not work. Evaluating (pairs integers integers) using this
;; definition causes infinite recursion, since evaluating interleave
;; causes its arguments to be evaluated, one of which is a recursive
;; call...and since we're dealing with infinite streams, this will
;; never terminate.

;;;;;;;;;
;;3.69
;;;;;;;;;

(define (triples s t u)
  (let ((t-u-pairs (pairs t u)))
    (cons-stream
     (cons (stream-car s) (stream-car t-u-pairs))
     (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
		  (stream-cdr t-u-pairs))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define t (triples integers integers integers))

(define (pythag-triples triples-stream)
  (stream-filter (lambda (x) (= (+ (square (car x))
				   (square (cadr x)))
				(square (caddr x))))
		 triples-stream))

(define (square x) (* x x))

(define py-trips (pythag-triples t))

;;;;;;;;;
;;3.70
;;;;;;;;;

(define (merge-weighted s1 s2 weight)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< (weight s1car) (weight s2car))
		  (cons-stream s1car
			(merge-weighted (stream-cdr s1) s2 weight)))
		 ((> (weight s1car) (weight s2car))
		  (cons-stream s2car
			(merge-weighted s1 (stream-cdr s2) weight)))
		 (else
		  (cons-stream s1car
			(merge-weighted (stream-cdr s1)
					s2
					weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; a.

(define weighted-pairs-by-sum (weighted-pairs integers integers
					      (lambda (x) (apply + x))))

;; b.

(define (not-divisible? n numlist)
  (if (null? numlist)
      #t
      (and (not (= (remainder n (car numlist)) 0))
	   (not-divisible? n (cdr numlist)))))


(define not-divisible-2-3-5 (stream-filter (lambda (x)
					     (not-divisible? x
							     (list 2 3 5)))
					   integers))

(define pairs-stream (weighted-pairs not-divisible-2-3-5
				     not-divisible-2-3-5
				     (lambda (x)
				       (+ (* 2 (car x))
					  (* 3 (cadr x))
					  (* 5 (car x)
					     (cadr x))))))

(display-stream pairs-stream 20)

;;;;;;;;;
;;3.71
;;;;;;;;;

(define (cube x) (* x x x))

(define (weight x)
  (+ (cube (car x))
     (cube (cadr x))))

(define pairs-stream (weighted-pairs integers integers weight))

(define (Ramanujan-pairs stream)
  (let ((s0 (stream-ref stream 0))
	(s1 (stream-ref stream 1)))
    (if (= (weight s0) (weight s1))
	(cons-stream s1 (Ramanujan-numbers
			 (stream-cdr (stream-cdr stream))))
	(Ramanujan-numbers (stream-cdr stream)))))

(define Ramanujan-numbers
  (stream-map weight (Ramanujan-pairs pairs-stream)))

(display-stream ram-nums 6)

;; The next five are: 4104, 13832, 20683, 32832, 39312.

;;;;;;;;;
;;3.72
;;;;;;;;;

(define (square x) (* x x))

(define (weight x)
  (+ (square (car x))
     (square (cadr x))))

(define pairs-stream (weighted-pairs integers integers weight))

(define (square-pairs stream)
  (let ((s0 (stream-ref stream 0))
	(s1 (stream-ref stream 1))
	(s2 (stream-ref stream 2))
	(rest (stream-cdr (stream-cdr (stream-cdr stream)))))
    (if (= (weight s0) (weight s1) (weight s2))
	(cons-stream (list s0 s1 s2) (square-pairs rest))
	(square-pairs (stream-cdr stream)))))

(define square-pairs-3-ways (stream-map (lambda (x)
					  (cons (weight (car x))
						x))
					(square-pairs pairs-stream)))

(display-stream square-pairs-3-ways 10)

;;;;;;;;;
;;3.73
;;;;;;;;;

(define (RC R C dt)
  (lambda (i v0)
    (let ((Ri (scale-stream i R)))
      (stream-map (lambda (x) (+ x v0))
		  (add-streams
		   (scale-stream (integral i 0 dt) (/ C))
		   Ri)))))

;;;;;;;;;
;;3.74
;;;;;;;;;

(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))

;;;;;;;;;
;;3.75
;;;;;;;;;

;; The problem with Louis's procedure is that it computes the average
;; by doing (x+y)/2 where x = (car input-stream) and y = the previous
;; average. This is obviously incorrect, as y should be the previous
;; value in the input stream instead.

(define (make-zero-crossings input-stream last-stream-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-stream-value) 2))
	(current (stream-car input-stream)))
    (cons-stream (sign-change-detector avpt last-avg)
		 (make-zero-crossings (stream-cdr input-stream)
				      current
				      avpt))))

;;;;;;;;;
;;3.76
;;;;;;;;;

(define (average x y)
  (/ (+ x y) 2))

(define (smooth s)
  (cond ((stream-null? s) the-empty-stream)
	((stream-null? (stream-cdr s)) (stream-car s))
	(else
	 (let ((s0 (stream-ref s 0))
	       (s1 (stream-ref s 1)))
	   (cons-stream (average s0 s1)
			(smooth (stream-cdr s)))))))

(define zero-crossings
  (stream-map sign-change-detector (smooth sense-data)
	      (stream-cdr (smooth sense-data))))

;;;;;;;;;
;;3.77
;;;;;;;;;
	      
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
	       (let ((integrand (force delayed-integrand))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt))))))

;;;;;;;;;
;;3.78
;;;;;;;;;

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
	       (scale-stream dy a)
	       (scale-stream y b)))
  y)

;; Note: The procedures in this section won't work in Scheme
;; implementations conforming to R5RS (that includes Scheme48, which
;; is what I'm using). See footnote 71, page 348.

;;;;;;;;;
;;3.79
;;;;;;;;;

(define (solve f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;;;;;;;;;
;;3.80
;;;;;;;;;

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0))
    (define dvC (scale-stream iL (- (/ C))))
    (define diL (add-streams (scale-stream iL (/ (- R) L))
			     (scale-stream vC (/ L))))
    (cons vC iL)))

;;;;;;;;;
;;3.81
;;;;;;;;;

;; Assuming the stream is a stream of lists, where we either get
;; (generate) or (reset n) where n is the seed used to reset our
;; random number generator.

(define (random-numbers stream rand-proc)
  (if (stream-null? (stream-car stream))
      the-empty-stream
      (let ((request (stream-car stream)))
	(cond ((eq? (car request) 'generate)
	       (cons-stream (rand-proc) (random-numbers (stream-cdr stream)
							rand-proc)))
	      ((eq? (car request) 'reset)
	       (cons-stream ((make-random (cadr request)))
			    (random-numbers (stream-cdr stream)
					    (make-random 5))))))))

;; Example using 5 as the seed.
(define randums (random-numbers request-stream (make-random 5)))

;;;;;;;;;
;;3.82
;;;;;;;;;

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; seed RNG with 5.
(define random (make-random 5))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (remainder (random) range))))

(define make-point cons)

(define (make-random-points x1 y1 x2 y2)
  (cons-stream (make-point (random-in-range x1 x2)
			   (random-in-range y1 y2))
	       (make-random-points x1 y1 x2 y2)))

(define (in-unit-circle? point)
  (let ((x (car point))
	(y (cdr point)))
    (<= (+ (expt (- x 5) 2) (expt (- y 7) 2)) 1)))

(define x1 4)
(define y1 6)
(define x2 6)
(define y2 8)

(define rectangle-area (* (- x2 x1) (- y2 y1)))

(define random-points (make-random-points x1 y1 x2 y2))

(define results-stream (stream-map in-unit-circle? random-points))

(define pi (stream-map (lambda (p) (/ (* rectangle-area p) 1.0))
		       (monte-carlo results-stream 0 0)))

(stream-ref pi 500)
