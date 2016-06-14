(library (qww6 future)
  (export (rename (%future future))
	  future? future-ready? touch)
  (import (chezscheme))
  (define-record-type future (fields mutex cv [mutable ready?] [mutable ->val]))
  (define (%future thunk)
    (define f (make-future (make-mutex) (make-condition) #f #f))
    (fork-thread
     (lambda ()
       (let ([->ret
	      (call/1cc
	       (lambda (k)
		 (with-exception-handler
		     (lambda (e) (k (lambda () (raise e))))
		   (lambda ()
		     (let ([res (thunk)])
		       (lambda () res))))))])
	 (with-mutex
	  (future-mutex f)
	  (future-->val-set! f ->ret)
	  (future-ready?-set! f #t)
	  (condition-broadcast (future-cv f))))))
    f)

  (define (touch f)
    (with-mutex
     (future-mutex f)
     (let loop ()
       (unless (future-ready? f)
	 (condition-wait (future-cv f) (future-mutex f))
	 (loop))))
    ((future-->val f))))
