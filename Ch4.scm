;;;;;;;;;
;;4.1
;;;;;;;;;

;; We can use LET for this:

;; Evaluating from left to right:

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
	(cons first
	      (list-of-values (rest-operands exps) env)))))

;; Evaluating from right to left:

(define (list-of-values exps env)
  (if (no-operands? (cdr exps))
      (list (eval (first-operand exps)))
      (let ((last (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env)
	      last))))

(define (test lst)
  (if (null? (cdr lst))
      (list (car lst))
      (let ((last (test (cdr lst))))
	(display (car lst))
	(cons (car lst) last))))
;;;;;;;;;
;;4.2
;;;;;;;;;

;; a.
;; 
;; Note that all (application? exp) does is check if exp is a pair,
;; and so nearly everything will count as an application (resulting in
;; an error eventually). In the case of evaluating (define x 3), this
;; is a pair, and thus will count as an application, resulting in the call
;; 
;; (apply (eval 'define env) (list-of-values (x 3) env))
;; 
;; which will obviously result in an error.

;; b.

;; The following changes are required for this:

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))

;;;;;;;;;
;;4.3
;;;;;;;;;

;; original EVAL
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Uknown expression type -- EVAL" exp))))

;; new EVAL
(define (install-eval-package)
  ;; Assume the various evaluation procedures are defined here

  (define (lambda-eval exp env)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))

  (define (begin-eval exp env)
    (eval-sequence (begin-actions exp) env))

  (define (cond-eval exp env)
    (eval (cond->if exp) env))

  (define (apply-eval exp env)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
  
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda lambda-eval)
  (put 'eval 'begin begin-eval)
  (put 'eval 'cond cond-eval)
  (put 'eval 'apply-eval apply-eval))

(define tag car)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((quoted? exp) (text-of-quotation exp))
	(else
	 (let ((func (get 'eval (tag exp))))
	   (cond (func (func exp env))
		 ((application? exp)
		  ((get 'eval 'apply-eval) exp env))
		 (else
		  (error "Uknown expression type -- EVAL" exp)))))))

;;;;;;;;;
;;4.4
;;;;;;;;;

;; As special forms

;; AND
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-eval-int predicates env)
  (cond ((null? predicates) #t)
	((not (car predicates)) #f)
	(and-eval-int (cdr predicates) env)))

(define (and-predicates exp)
  (cdr exp))

(define (and-eval exp env)
  (and-eval-int (and-predicates exp) env))

;; OR
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-eval-int predicates env)
  (cond ((null? predicates) #f)
	((car predicates) #t)
	(or-eval-int (cdr predicates) env)))

(define (or-predicates exp)
  (cdr exp))

(define (or-eval exp env)
  (or-eval-int (or-predicates exp) env))

;; As derived expressions - derived from IF:

;; AND
(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))

(define (expand-and-predicates predicates)
  (if (null? predicates)
      #t
      (let ((first (car predicates))
	    (rest (cdr predicates)))
	(make-if (not first)
		 #f
		 (expand-and-predicates rest)))))

(define (and-eval exp env)
  (eval (and->if exp) env))


;; OR

(define (or->if exp)
  (expand-or-predicates (or-predicates exp)))

(define (expand-or-predicates predicates)
  (if (null? predicates)
      #f
      (let ((first (car predicates))
	    (rest (cdr predicates)))
	(make-if first
		 #t
		 (expand-or-predicates rest)))))

(define (or-eval exp env)
  (eval (or->if exp) env))

;;;;;;;;;
;;4.5
;;;;;;;;;

(cond ((assoc 'b '((a 1) (d 2))) => cadr)
      ((= 1 1) #t)
      (else #f))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (let ((actions (cond-actions first)))
	      (if (eq? (cadr actions) '=>)
		  (make-if (cond-predicate first)
			   (make-lambda '()
					(list (list (caddr actions)
						    (cond-predicate
						     first))))
			   (expand-clauses rest)))
		  (make-if (cond-predicate first)
			   (sequence->exp actions)
			   (expand-clauses rest)))))))

;;;;;;;;;
;;4.6
;;;;;;;;;

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let->combination exp)
  (let ((vars (map car (cadr exp)))
	(exps (map cadr (cadr exp)))
	(body (caddr exp)))
    (cons (make-lambda vars (list body))
	  exps)))

(define (let? exp)
  (tagged-list? exp 'let))

;; and EVAL now becomes

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((let? exp)
	 (eval (let->combination exp) env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Uknown expression type -- EVAL" exp))))

;;;;;;;;;
;;4.7
;;;;;;;;;

;; One could start a new nesting for each succeeding binding. For example:

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

;; becomes


(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))


(define (let*->nested-lets exp)
  (let ((bindlist (cadr exp))
	(body (caddr exp)))
    (define (make-nested-lets bindings)
      (if (null? bindings)
	  body
	  (append (list 'let (list (car bindings)))
		  (list (make-nested-lets (cdr bindings))))))
    (make-nested-lets bindlist)))

;; Yes, adding a class to EVAL whose action is
;;
;; (eval (let*->nested-lets exp) env)
;;
;; is sufficient, as we go from LET* to nested LET's to nested
;; LAMBDA's, which is fine.

;;;;;;;;;
;;4.8
;;;;;;;;;

;; if (cadr exp) is a symbol, then we have a named LET, otherwise we
;; have regular LET.

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let ((procname (cadr exp))
	    (vars (map car (caddr exp)))
	    (exps (map cadr (caddr exp)))
	    (body (cdddr exp)))
	(define procname (make-lambda vars body))
	(cons procname exps))
      (let ((vars (map car (cadr exp)))
	    (exps (map cadr (cadr exp)))
	    (body (cddr exp)))
	(cons (make-lambda vars body)
	      exps))))
	
;;;;;;;;;
;;4.9
;;;;;;;;;

;; We could have a procedure do-from-to, whose signature is
;; (do-from-to start end body) which repeats BODY in a loop starting
;; from START up to but not including END.

(define (transform-do-from-to exp)
  (let ((start (cadr exp))
	(end (caddr exp))
	(body (cadddr body)))
    (define internal (make-lambda '(count) (make-if (list '< 'count end)
						    (list body)
						    (list 'internal (+
								     'count
								     1)))))
    (list internal start)))

;; and we'd add this clause to EVAL:

(define (do-from-to? exp)
  (tagged-list? exp 'do-from-to))

;;;;;;;;;
;;4.10
;;;;;;;;;

;; You could do anything you want really, since we're just changing
;; implementation, not interface. e.g. you could use "=" instead of
;; DEFINE, and DEFINITION? would be defined as:

;; (define (definition? exp)
;;  (tagged-list? exp '=))

;; and of course you'd then lose the arithmetic "=", so maybe you
;; could change your arithmetic to be infix or postfix or something.

;;;;;;;;;
;;4.11
;;;;;;;;;

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame)
  (map car frame))

(define (frame-values frame)
  (map cdr frame))

(define (prepend! lst x)
  (set-cdr! lst (cons (car lst) (cdr lst)))
  (set-car! lst x))

(define (add-binding-to-frame! var val frame)
  (prepend! frame (cons var val)))

(define frame (make-frame (list 'a 'b 'c) (list 1 2 3)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((assoc var frame) => cdr)
	    (else (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((binding (assoc var frame)))
	(if binding
	    (set-cdr! binding val)
	    (env-loop (enclosing-environment env)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan myframe)
      (let ((binding (assoc var myframe)))
	(if binding
	    (set-cdr! binding val)
	    (add-binding-to-frame! var val myframe))))
    (scan frame)))

;;;;;;;;;
;;4.12
;;;;;;;;;

;; something like this? I'm dubious...this is way too awkward.

;; using book's representation of frames as pairs of lists
(define (find-apply var val vars vals proc)
  (cond ((null? vars) 'not-present)
	((eq? var (car vars)) (proc vals val))
	(else (find-apply var val (cdr vars) (cdr vals) proc))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let* ((frame (first-frame env))
	       (result (find-apply var '() (frame-variables frame)
				   (frame-values frame)
				   (lambda (x y) (car x)))))
	  (if (eq? result 'not-present)
	      (env-loop (enclosing-environment env))
	      result))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let* ((frame (first-frame env))
	       (result (find-apply var val (frame-variables frame)
				   (frame-values frame)
				   set-car!)))
	  (if (eq? result 'not-present)
	      (env-loop (enclosing-environment env))
	      result))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
	 (result (find-apply var val (frame-variables frame)
			     (frame-values frame)
			     set-car!)))
    (if (eq? result 'not-present)
	(add-binding-to-frame! var val frame)
	result)))

;;;;;;;;;
;;4.13
;;;;;;;;;

;; We check for the variable binding. If we do find it, then we remove
;; that binding from our frame, i.e. remove the var and the val from
;; their respective lists. We only remove the binding of the name
;; bound in the closest frame. This is consistent, because it's
;; similar to the scope rules. When we want to resolve a name, we
;; first check in the environment that we are in, then in the
;; enclosing environment, etc. As soon as we find the name, we stop
;; looking. Same thing here.
;;
;; (again, going with the book's representation of frames where each
;; frame is a pair of lists)

(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vars (cadr vars))
	     (set-cdr! vars (cddr vars))
	     (set-car! vals (cadr vals))
	     (set-cdr! vals (cddr vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable")
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;;;;;;;;;
;;4.14
;;;;;;;;;

;; Let's trace what happens when we apply (map (lambda (x) x) mylist):
;; 
;; First, EVAL sees that it is an application, and so it calls
;; 
;; (apply (eval (operator exp) env)
;;        (list-of-values (operands exp) env))
;; 
;; which then calls
;; 
;; (apply-primitive-procedure map ('procedure (x) x env) mylist)
;; 
;; APPLY-PRIMITIVE-PROCEDURE simply defers to the underlying
;; implementation to do the application. But the underlying
;; implementation (in this case Scheme) will choke when it gets to
;; ('procedure ...). 
;; 
;; The lesson to take away from this is that procedures are
;; represented differently in our meta evaluator compared to how they
;; are represented in the underlying implementation.

;;;;;;;;;
;;4.15
;;;;;;;;;

;; Proof by contradiction:
;; 
;; Suppose (try try) halts. Then, it must be the case that (halts? try
;; try) evaluated to false. But this should cause (try try) to run
;; forever. Contradiction. 
;; 
;; Now suppose (try try) runs forever. Then, it must be the case that
;; (halts? try try) evaluated to false. But this should cause (try
;; try) to halt by returning 'halted. Contradiction.

;;;;;;;;;
;;4.16
;;;;;;;;;

;; a.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (if (eq? (car vals) '*unassigned)
		 (error "Unassigned variable" var)
		 (car vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
    (env-loop env))

;; b.

(define (scan-out-defines procbody)
  (define (get-vars-exps body)
    (let ((first (car body)))
      (if (eq? (car first) 'define)
	  (cons (list (cadr first) (caddr first)) (get-vars-exps (cdr body)))
	  '())))
  (define (get-rest body)
    (if (eq? (caar body) 'define)
	(get-rest (cdr body))
	body))
  (let* ((vars-exps (get-vars-exps procbody))
	 (vars (map car vars-exps))
	 (exps (map cadr vars-exps))
	 (rest (get-rest procbody)))
    (cons 'let
	  (cons (map (lambda (var) (list var '*unassigned*))
		     vars)
		(append (map (lambda (var-exp-pair)
			       (cons 'set! var-exp-pair))
			     vars-exps)
			rest)))))

;; c.

;; MAKE-PROCEDURE is probably better, so it becomes something like

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; It's better here than in PROCEDURE-BODY because it's better to get
;; the processing out of the way at the time of construction, instead
;; of every single time the PROCEDURE-BODY accessor is called.

;;;;;;;;;
;;4.17
;;;;;;;;;

;; We have an extra frame because LET creates a new environment. This
;; doesn't affect the behaviour of a correct program because it still
;; is the case that right before <e3>, u is bound to <e1> and v is
;; bound to <e2>. Another way to achieve the same affect is to
;; tranform the expression such that the LET becomes two DEFINE's,
;; i.e.
;; 
;; (define u '*unassigned*)
;; (define v '*unassigned*)
;; 
;; followed by the two SET!'s.
;; 
;; Alternatively, you can have two extra variables u and v in the
;; lambda parameter list that serve as auxiliary variables, and have
;; them auto-initialised to '*unassigned*, and then SET! can be called
;; in the body of the lambda, something like:
;; 
;; (lambda (var1 var2 var3 #aux (u '*unassigned*) (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>))
;; 
;; where #aux is just a sentinel indicating auxiliary variables
;; (thanks to Riastradh on Freenode's #scheme for pointing out that
;; this is how MIT Scheme does it).
;;

;;;;;;;;;
;;4.18
;;;;;;;;;

;; No, it will not. Realise that when <e2> is evaluated, U still has
;; '*UNASSIGNED* bound to it, and so (STREAM-MAP F U) won't work.
;; 
;; If they are scanned out as shown in the text, this procedure will
;; work. This is because U will already have 
;;
;; (INTEGRAL (DELAY U) Y0 DT) 
;; 
;; bound to it by the time <e2>, i.e. (STREAM-MAP F U) is evaluated.

;;;;;;;;;
;;4.19
;;;;;;;;;

;; I support Alyssa's viewpoint. While Eva says that due to
;; simultaneous definitions A should be 5 and B should be 15, to me
;; that still sounds sequential, i.e. (DEFINE A 5) was evaluated
;; before (DEFINE B (+ A X)). It seems somewhat difficult to consider
;; the true meaning of simultaneous definition when mutual recursion
;; is involved, so as the book says, better to generate an error
;; condition than to do something incorrect (e.g. Ben).

;;;;;;;;;
;;4.20
;;;;;;;;;

;; a.

(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
	(exps (map cadr (cadr exp)))
	(rest (cddr exp)))
    (append (list 'let (map (lambda (var) (list var '*unassigned*)) vars))
	    (map (lambda (var exp) (list 'set! var exp)) vars exps)
	    rest)))

;; b.

;; Environment diagram not drawn here, but the key point here is in
;; the definition of LETREC on page 392: "the expressions exp_k that
;; provide the initial values for the variables var_k are evaluated in
;; an environment that includes all LETREC bindings". This is not the
;; case for LET, and so if we use LET in place of LETREC, then the
;; call to ODD? in the definition of EVEN? will cause Scheme to look
;; up ODD? in the enclosing environment, as opposed to a current environment
;; that includes all the LET bindings, i.e. including the intended
;; ODD? that refers back to EVEN?.

;; (something to note: R5RS defines both EVEN? and ODD?, so perhaps
;; rename them if you're testing this procedure).

;;;;;;;;;
;;4.21
;;;;;;;;;

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
	  1
	  (* k (ft ft (- k 1)))))))
 5)

;; a.

((lambda (n)
   ((lambda (fib)
      (fib fib 0 1 n))
    (lambda (fb a b count)
      (if (= count 0)
	  b
	  (fb fb b (+ a b) (- count 1))))))
 5)

;; b.

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

;;;;;;;;;
;;4.22
;;;;;;;;;

;; Analogous to 4.6...have a LET->COMBINATION and the appropriate entry
;; in ANALYZE.

;;;;;;;;;
;;4.23
;;;;;;;;;

;; As mentioned in the book, in Alyssa's version we have an extra
;; structure (the actual sequence), as opposed to only the procedures
;; we want to call. The extra work to be done in Alyssa's version is
;; that we need to traverse this sequence in addition to calling each
;; procedure, whereas in the version shown in the book, we do not have
;; any extra overhead, yet still call each procedure in sequence.

;;;;;;;;;
;;4.24
;;;;;;;;;

;; Can't be bothered to download and load the code right now, maybe
;; later...it's clear enough that the analysing evaluator is faster.

;;;;;;;;;
;;4.25
;;;;;;;;;

;; Attempting to evaluate (FACTORIAL 5) will result in infinite
;; recursion, since the recursive call in UNLESS will always be
;; evaluated no matter what n is. Our definition would work in a
;; normal-order language.

;;;;;;;;;
;;4.26
;;;;;;;;;

;; The usual drill...

(define (unless->if exp)
  (let ((condition (cadr exp))
	(usual-value (caddr exp))
	(exceptional-value (cadddr exp)))
    (list 'if condition exceptional-value usual-value)))

;; Not sure what more there is to add to the argument, seems like both
;; main points were covered. Can't really think of a situation where
;; it would be useful to have UNLESS available as a procedure, except
;; some contrived examples involving higher-order procedures.

;;;;;;;;;
;;4.27
;;;;;;;;;

;;; L-Eval input:
;; count
;;; L-Eval value:
1

;;; L-Eval input:
;; w
;;; L-Eval value:
10

;;; L-Eval input:
;; count
;;; L-Eval value:
2

;;;;;;;;;
;;4.28
;;;;;;;;;

;; Imagine passing a procedure as an argument to another one, and
;; applying it internally.

;;;;;;;;;
;;4.29
;;;;;;;;;

;; Without memoisation:

;;; L-Eval input:
;; (square (id 10))
;;; L-Eval value:
;; 100

;;; L-Eval input:
;; count
;;; L-Eval value:
;; 2

;; With memoisation:

;;; L-Eval input:
;; (square (id 10))
;;; L-Eval value:
;; 100

;;; L-Eval input:
;; count
;;; L-Eval value:
;; 1

;;;;;;;;;
;;4.30
;;;;;;;;;

;; a.

;; Ben is right in this case because the procedure PROC is passed as a
;; parameter to FOR-EACH (thus delayed), and so when it's time for the call 
;; (PROC (CAR ITEMS)), the evaluator will recognise that this is an
;; application, and so PROC will be forced, then applied.

;; b.

;; This is different to the situation in part a) because here, the
;; procedure _call_ is the argument that is delayed. Thus when we call
;; EVAL on the variable E, we get back the thunk, but it is not forced.

;; With the original EVAL-SEQUENCE:
;; 
;; (p1 1) => (1 2)
;; (p2 1) => 1

;; With Cy's EVAL-SEQUENCE:
;; 
;; (p1 1) => (1 2)
;; (p2 1) => (1 2)

;; c.

;; The crucial point to realise here is that FORCE-IT will yield the same
;; object it is called on if it is not a thunk.  Thus in this case, 
;; even though the call (PROC (CAR ITEMS)) is not delayed, it is still
;; safe to call ACTUAL-VALUE on it, as Cy's EVAL-SEQUENCE would do.  

;; d.

;; I prefer the approach in the text for two reasons:
;; 
;; 1) It doesn't compute more than the programmer needs.  For example,
;; consider again the procedure P2 in part b).  Suppose we called 
;; (P2 (CALCULATE-BILLIONTH-PRIME)).  The value of the variable E
;; isn't returned (X is the one returned), and so we don't even need
;; to evaluate the call to (CALCULATE-BILLIONTH-PRIME).
;; 
;; 2) It doesn't allow side-effect operations in sequences.  We saw
;; earlier how confusing side-effects can be.
;; 
;; I find Cy's approach to be clumsy.  It's too "brute-force", and
;; ignores the two problems described above that the book's approach
;; deals with.
;; 
;; Perhaps there's an approach that is better than both?  Monads like
;; in Haskell, perhaps (I don't know what they are yet)?

;;;;;;;;;
;;4.31
;;;;;;;;;



;;;;;;;;;
;;4.35
;;;;;;;;;

(define (an-integer-between low high)
	       (require (< low high))
	     (either low (an-integer-between (+ low 1) high)))

;;;;;;;;;
;;4.39
;;;;;;;;;

(define (require p)
  (if (not p)
      (either)))

(define (distinct? items)
  (cond ((null? items) #t)
	((null? (cdr items)) #t)
	((member (car items) (cdr items)) #f)
	(else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (either 1 2 3 4 5))
	(cooper (either 1 2 3 4 5))
	(fletcher (either 1 2 3 4 5))
	(miller (either 1 2 3 4 5))
	(smith (either 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= cooper 1)))
    (require (not (= baker 5)))
    (require (> miller cooper))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

(define (calltimes proc n)
  (if (= n 1)
      (with-nondeterminism (lambda ()
			     (proc)))
      (begin
	(with-nondeterminism (lambda ()
			       (proc)))
	(calltimes proc (- n 1)))))

(define (time proc n)
  (let ((begin-time (real-time)))
    (let ((result (calltimes proc n)))
      (let ((end-time (real-time)))
	(display "Time taken: ")
	(display (exact->inexact (/ (- end-time begin-time) n)))
	(newline)
	result))))

;; Is this faster?

(define (multiple-dwelling)
  (let ((baker (either 1 2 3 4 5))
        (cooper (either 1 2 3 4 5))
        (fletcher (either 1 2 3 4 5))
        (miller (either 1 2 3 4 5))
        (smith (either 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= cooper 1)))
    (require (not (= baker 5)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
