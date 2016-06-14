;;;lambda with optional parameters
;;;example : (lambda+ (a b [c 1] [d 2] . rest) (list a b c d rest))

(library (qww6 lambda+)
  (export lambda+ define+)
  (import (chezscheme))
  
  (define-syntax (lambda+ stx)
    (define (syntax->list* thing)
      (let loop ([thing thing])
	(syntax-case thing ()
	  [(A . B) (cons #'A (loop #'B))]
	  [() '()]
	  [T #'T])))
    
    (define (filter-split* proc ls)
      (let loop ([ls ls] [acc '()])
	(cond
	 [(not (pair? ls)) (values (reverse acc) ls)]
	 [(proc (car ls)) =>
	  (lambda (thing)
	    (loop (cdr ls) (cons thing acc)))]
	 [else
	  (values (reverse acc) ls)])))

    (define (partition-formals Fmls)
      (define fmls (syntax->list* Fmls))
      (define-values (fixed* opt+rest)
	(filter-split* (lambda (c) (and (identifier? c) c)) fmls))
      (define-values (opt* rest*)
	(filter-split* (lambda (slot)
			 (syntax-case slot ()
			   [(Id Default) slot]
			   [else #f]))
		       opt+rest))
      (define rest
	(cond
	 [(null? rest*) #f]
	 [(identifier? rest*) rest*]
	 [else (syntax-violation 'lambda+ "invalid lambda form" rest)]))
      (values fixed* opt* rest `(,@fixed* ,@(map (lambda (slot)
						   (syntax-case slot ()
						     [(Id Default) #'Id]))
						 opt*)
					  ,@(if rest (list rest) '()))))
    
    (syntax-case stx ()
      [(lambda+ Fmls Body Body* ...)
       (let-values ([(fixed* opt* rest plain*) (partition-formals #'Fmls)])
	 (with-syntax ([(Fixed ...) fixed*]
		       [(Opt ...) opt*]
		       [Rest rest]
		       [(Plain ...) plain*])
	   #'(letrec ([f (lambda (Plain ...) Body Body* ...)])
	       (lambda+dispatch f (Fixed ...) (Opt ...) Rest))))]))

  (define-syntax (lambda+dispatch stx)
    (define (clauses stx)
      (syntax-case stx ()
	[(F #f (Fixed ...) ())
	 (list #'((Fixed ...) (F Fixed ...)))]
	[(F #f (Fixed ...) ([Id Default] [Id* Default*] ...))
	 (cons #'((Fixed ...) (F Fixed ... Default Default* ...))
	       (clauses #'(F #f (Fixed ... Id) ([Id* Default*] ...))))]
	
	[(F Rest (Fixed ...) ())
	 (with-syntax ([R (datum->syntax #'F (gensym))])
	   (list #'((Fixed ... . R) (F Fixed ... R))))]
	[(F Rest (Fixed ...) ([Id Default] [Id* Default*] ...))
	 (cons #'((Fixed ...) (F Fixed ... Default Default* ... '()))
	       (clauses #'(F Rest (Fixed ... Id) ([Id* Default*] ...))))]))
    (syntax-case stx ()
      [(_ F (Fixed ...) (Opt ...) Rest)
       (with-syntax ([(Clauses ...) (clauses #'(F Rest (Fixed ...) (Opt ...)))])
	 #'(case-lambda
	     Clauses ...))]))

  (define-syntax define+
    (syntax-rules ()
      [(_ (Id . Fml) Body Body* ...)
       (define* Id (lambda+ Fml Body Body* ...))]
      [(_ Id Expr)
       (define Id Expr)])))
