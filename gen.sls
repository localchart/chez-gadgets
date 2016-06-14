(library (qww6 gen)
  (export generator)
  (import (chezscheme))
  
  (define (generator fn)
    (define restore-cc #f)
    (define yield-cc #f)
    (lambda ()
      ((call/1cc
	(lambda (k)
	  (set! yield-cc k)
	  (cond
	   [restore-cc (restore-cc)]
	   [else
	    (with-exception-handler
		(lambda (x) (yield-cc (lambda () (raise x))))
	      (lambda ()
		(yield-cc
		 (let* ([last
			 (fn (lambda (v)
			       (call/1cc
				(lambda (cc)
				  (set! restore-cc cc)
				  (yield-cc (lambda () v))))))]
			[final (lambda () last)])
		   (set! restore-cc
			 (lambda () (yield-cc final)))
		   final))))]))))))
  ;;;example
  #|
  (define (range n)
    (generator
     (lambda (yield)
       (let loop ([i 0])
	 (when (< i n)
	   (yield i)
	   (loop (+ i 1)))))))
|#
  )
