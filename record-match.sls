(library (qww6 record-match)
  (export record-match)
  (import (chezscheme))
  
  (define-syntax (record-match stx)
    (define (generate-accessor Name Id* Thing)
      (let loop ([Id* Id*] [Bind* '()] [n 0])
	(syntax-case Id* ()
	  [() (reverse Bind*)]
	  [((Field Id) Rest ...)
	   (loop #'(Rest ...)
		 (cons #`(Id ((csv7:record-field-accessor
			       (record-type-descriptor #,Name)
			       'Field)
			      #,Thing))
		       Bind*)
		 #f)]
	  [(Id Rest ...)
	   (if n
	       (loop #'(Rest ...)
		     (cons #`(Id ((record-accessor
				   (record-type-descriptor #,Name)
				   #,(datum->syntax #'k n))
				  #,Thing))
			   Bind*)
		     (+ n 1))
	       (syntax-violation 'record-match
				 "invalid indexed field after named one" #'Id))])))
    (syntax-case stx (else)
      [(_ (Expr ...) Clauses ...)
       #'(let ([v (Expr ...)])
	   (record-match v Clauses ...))]
      [(_ Thing)
       #'(error 'record-match "match fail")]
      [(_ Thing [else Body ...])
       #'(let () Body ...)]
      [(_ Thing [(Name Id ...) Body ...] Rest ...)
       (with-syntax ([(Bind* ...) (generate-accessor #'Name #'(Id ...) #'Thing)])
	 #'(if ((record-predicate (record-type-descriptor Name)) Thing)
	       (let (Bind* ...)
		 Body ...)
	       (record-match Thing Rest ...)))])))
