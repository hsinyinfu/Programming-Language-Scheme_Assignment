;;; Scheme in Scheme, Mini-Scheme Version 2
;;; enter (exit) to quit
(define (scheme) ;;; the read-eval-print loop.
  (display "myScheme>") 
  (let ((expr (read)))
    (cond ((equal? expr '(exit))      ; (exit) only allowed at top level
	   'done)
	  (else  (display (top-eval expr))
		 (newline)
		 (scheme))
	  )))

;;; not needed for assignment
(define (my-load filename)       ;; don't want to redefine the Scheme LOAD
  (load-repl (open-input-file filename)))

(define (load-repl port)  ;read-eval-print-loop: repl
  (let ((expr (read port)))
    (cond ((eof-object? expr) 'done)
	  (else (let ((res (top-eval expr)))
		  (display res) (newline)
		  (load-repl port)))
	  )))

;; (define ....) is only allowed at the top level and affects only the 
;; global environment. Only the basic form of define is supported here.

(define (top-eval expr)
  (cond ((not (pair? expr)) (my-eval expr globals))
	((eq? (car expr) 'define)         ;LOOK CAREFULLY HERE.
         (set! globals (cons (list (cadr expr) (my-eval (caddr expr) globals)) globals))
         )     
	(else (my-eval expr globals))
	))
		  

;;; Representation of non-primitive function: (%func args body env)
(define make-func
  (lambda (var-list body env)
    (list '%func var-list body env)))
    
	
;; display error message
(define (error msg s) (display msg))

;; Global environment
(define globals
  (list
    (list `display display)
    (list `< <)
    (list `- -)
    (list `/ /)
    (list '+ +)
    (list '* *)
    (list '= =)
    (list '> >)
    (list 'fact (make-func '(n) '(if (= n 0) 1 (* n (fact (+ n -1)))) '()) )
    (list 'car car)
    (list 'cdr cdr)
    (list 'null? null?)
    (list 'cons cons)
    (list 'eq? eq?)
    (list 'pair? pair?)
    (list 'not (make-func '(x) '(eq? x #f) '()) )
    (list 'equal? (make-func '(a b)
			'(if (pair? a)
			     (if (pair? b)
				 (if (equal? (car a) (car b))
				     (equal? (cdr a) (cdr b))
				     #f)
				 #f)
			     (if (pair? b)
				 #f
				 (eq? a b)))
			'()))
    ))

;;; Representation of environment: association list.
;; ((name_1 value_1) ... (name_n value_n))

;;; To look up a variable, first look in "local" environment, then in
;;; global invironment.
(define lookup-symbol
  (lambda (s env)
    (cond ((assoc s env) => cadr)
	  ((assoc s globals) => cadr)
	  (else (error "symbol not found:" s)))))
	  

   
;;Helper fun for constructing environments
(define (zip z lst1 lst2)
  (if (null? lst1)
      '()
      (cons 
        (z (car lst1) (car lst2))
        (zip z (cdr lst1) (cdr lst2)))))
        
;; extend an environment
(define (extend-env vars vals env)
   (append (zip list vars vals) env))	
   
;; Predicates for checking forms of Expressions         
(define selfevaluating?
  (lambda (expr)
    (not (pair? expr))))
  
(define lambda? 
  (lambda (expr)
    (eq? (car expr) 'lambda)))

(define quote?
  (lambda (expr)
    (eq? (car expr) 'quote)))

(define if?
  (lambda (expr)
	(eq? (car expr) 'if)))

(define cond?
  (lambda (expr)
	(eq? (car expr) 'cond)))
	
(define let?
  (lambda (expr)
	(eq? (car expr) 'let)))

(define let*?
  (lambda (expr)
	(eq? (car expr) 'let*)))	

(define letrec?
  (lambda (expr)
	(eq? (car expr) 'letrec)))


;;main evaluating function

(define my-eval
  (lambda (expr env)
    (cond ((symbol? expr)	;normal variable
	   (lookup-symbol expr env))
	  ((selfevaluating? expr) expr)	;self evaluating
	  ;;Special forms: LAMBDA, QUOTE, IF
          ((lambda? expr)	;LAMBDA
              (make-func (cadr expr) (caddr expr) env))
          ((quote? expr)	;QUOTE
	      (cadr expr))
          ((if? expr)		;IF
	      (my-eval ((if (my-eval (cadr expr) env)
		            caddr
			    cadddr)
			           expr)
			     env))
	  ((cond? expr)          ;COND
           (my-cond-eval (cadr expr) env)
           ) 		     
	     ; (display "*Error: cond not ready yet:") #f )		     
          ((let? expr)	;LET
	      (let* ( (clauses (cadr expr))
	    	      (body (caddr expr))
		      (vars (map car clauses))
		      (exprs (map cadr clauses))
	              (vals (map (lambda (e) (my-eval e env)) exprs))
	              (new-env (append (zip list vars vals) env)) )     
		  (my-eval body new-env)))
	  ((let*? expr)  ; LET*
	    (my-let*-eval expr env)  ) 
	  ((letrec? expr)  ; LETREC
	    (error "letrec not ready yet:" expr)  )    
          (else   ;regular funcall
              (let ( (eexpr (map (lambda (e) (my-eval e env))
				  expr)))
		  (my-apply (car eexpr) (cdr eexpr)))))))
		  
;; Apply a function to its arguments
(define my-apply
  (lambda (f args)
    (cond ((procedure? f)		; primitive function
	   (apply f args))
	  ((and (pair? f) (eq? (car f) '%func)) ; non-primitive function
	   (my-eval (caddr f) ; the proc body
	        (extend-env (cadr f) ;parameters
                        args 
						(cadddr f)) ;env-in-closure
		))
	  (else (error "invalid function in MY-APPLY" f)))))

(define my-cond-eval
  (lambda (expr env)
    (if (eq? `else (car (car expr))) (my-eval(cadar expr) env)
        (if (my-eval (car (car expr)) env) (my-eval (cadr (car expr)) env) (my-eval (list `cond (cdr expr)) env)) )
   ))

(define (my-let*-eval expr env)
  (define (let-binding bindingExpr)
    (if (null? bindingExpr) `() (cons (list (car (car bindingExpr)) (my-eval (cadar bindingExpr) env)) (let-binding (cdr bindingExpr)))))
  (my-eval (caddr expr) (append (let-binding (cadr expr)) env))
  )
;; Starting the mini-scheme interpreter
(scheme)
