(library (qww6 for)
  (export for for* for/fold for*/fold
	  in-range in-vector in-list in-hash in-naturals
	  define-for-clauses expand-for-clauses)
  (import (chezscheme))
  
  (meta define for-clauses '())

  (meta define (add-for-clauses! t)
	(set! for-clauses (cons t for-clauses)))
  
  (meta define (expand-for-clauses stx)
	(syntax-case stx ()
	  [(T Expr ...)
	   (let ([slot (assp (lambda (f) (free-identifier=? f #'T))
			     for-clauses)])
	     (if slot
		 ((cdr slot) stx)
		 (errorf 'expand-for-clauses "unrecognized type")))]))

  (meta define (add-paren Var)
    (syntax-case Var ()
      [(Id ...) #'(Id ...)]
      [Var #'(Var)]))
  
  (define-syntax (for stx)
    (syntax-case stx ()
      [(_ ([Var Expr] ...) Body ...)
       (let ()
	 (with-syntax ([(Var ...) (map add-paren (syntax->list #'(Var ...)))]
		       [((init first get rest empty) ...)
			(map expand-for-clauses #'(Expr ...))])
	   #'(let-values (init ...)
	       (let loop (first ...)
		 (if (or empty ...)
		     (void)
		     (let-values ([Var get] ...)
		       Body ...
		       (loop rest ...)))))))]))

  (define-syntax for*
    (syntax-rules ()
      [(_ (Bind Bind* ...) Body ...)
       (for (Bind) (for* (Bind* ...) Body ...))]
      [(_ () Body ...)
       (let () Body ...)]))

  (define-syntax (for/fold stx)
    (syntax-case stx ()
      [(_ ([Id Init] ...) ([Var Expr] ...) Body ...)
       (let ()
	 (with-syntax ([(Var ...) (map add-paren (syntax->list #'(Var ...)))]
		       [((init first get rest empty) ...)
			(map expand-for-clauses #'(Expr ...))])
	   #'(let ([Id Init] ...)
	       (let-values (init ...)
		 (let loop ([Id Id] ... first ...)
		   (if (or empty ...)
		       (values Id ...)
		       (let-values ([Var get] ...)
			 (let-values ([(Id ...) (let () Body ...)])
			   (loop Id ... rest ...)))))))))]))

  (define-syntax for*/fold
    (syntax-rules ()
      [(_ ([Var Init] ...) (Bind Bind* ...) Body ...)
       (for/fold ([Var Init] ...) (Bind)
		 (for*/fold ([Var Var] ...) (Bind* ...) Body ...))]      
      [(_ ([Var Init] ...) () Body ...)
       (let ([Var Init] ...) Body ...)]))
  
  (define-syntax define-for-clauses
    (syntax-rules  ()
      [(_ (Id Stx) Expr)
       (define-for-clauses Id (lambda (Stx) Expr))]
      [(_ Id Expr)
       (meta
	define Id 
	(let ([slot (assq (lambda (f) (free-identifier=? #'Id f)) for-clauses)]
	      [proc Expr])
	  (if slot
	      (set-cdr! slot proc)
	      (add-for-clauses! (cons #'Id proc)))
	  proc))]))

  (define-for-clauses (in-range Bind)
    (let rec ([Bind Bind]) 
      (syntax-case Bind (in-range)
	[(in-range Expr)
	 (rec #'(in-range 0 Expr 1))]
	[(in-range Start End)
	 (rec #'(in-range Start End 1))]
	[(in-range Start End Step)
	 (with-syntax ([(i start end step)
			(generate-temporaries #'(Start Start End Step))])
	   (list #'((start end step) (values Start End Step))
		 #'(i start)
		 #'i
		 #'(+ step i)
		 #'(>= i end)))])))

  (define-for-clauses (in-naturals Bind)
    (let rec ([Bind Bind])
      (syntax-case Bind (in-naturals)
	[(in-naturals Start)
	 (with-syntax ([(start i) (generate-temporaries '(1 1))])
	   (list #'((start) Start)
		 #'(i start)
		 #'i
		 #'(+ i 1)
		 #'#f))]
	[(in-naturals)
	 (rec #'(in-naturals 0))])))

  (define-for-clauses (in-list Bind)
    (syntax-case Bind (in-list)
      [(in-list Ls)
       (with-syntax ([(i start) (generate-temporaries #'(Ls Ls))])
	 (list #'((start) Ls)
	       #'(i start)
	       #'(car i)
	       #'(cdr i)
	       #'(null? i)))]))

  (define-for-clauses (in-vector Bind)
    (syntax-case Bind (in-vector)
      [(in-vector Vec)
       (with-syntax ([(i start len) (generate-temporaries #'(Vec Vec Vec))])
	 (list #'((vec len) (let ([v  Vec]) (values v (vector-length v))))
	       #'(i 0)
	       #'(vector-ref vec i)
	       #'(1+ i)
	       #'(= i len)))]))

  (define-for-clauses (in-hash Bind)
    (syntax-case Bind (in-hash)
      [(in-hash Hash)
       (with-syntax ([(i start len) (generate-temporaries #'(Hash Hash Hash))])
	 (list #'((k v len)
		  (let-values ([(k  v) (hashtable-entries Hash)])
		    (values k v (vector-length v))))
	       #'(i 0)
	       #'(values (vector-ref k i) (vector-ref v i))
	       #'(1+ i)
	       #'(= i len)))]))


  )
