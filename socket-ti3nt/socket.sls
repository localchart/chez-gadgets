(library (qww6 socket)
  (export make-client-socket make-server-socket
	  socket?
	  
	  socket-input-port socket-output-port
	  call-with-socket
	  
	  socket-merge-flags socket-purge-flags
	  
	  socket-accept socket-send socket-recv socket-shutdown socket-close
	  socket-send! socket-recv!
	  
	  *af-unspec* *af-inet* *af-inet6*
	  *sock-stream* *sock-dgram*
	  *ai-canonname* *ai-numerichost*
	  *ai-v4mapped* *ai-all* *ai-addrconfig*
	  *ipproto-ip* *ipproto-tcp* *ipproto-udp*
	  *shut-rd* *shut-wr* *shut-rdwr*
	  
	  address-family socket-domain address-info
	  ip-protocol message-type shutdown-method)
  (import (chezscheme) (qww6 opt))


  (define dummy
    (case (machine-type)
      [(ti3nt)
       (load-shared-object "ws2_32.dll")
       (cond
	[(getenv "SOCKET_SO_PATH")
	 =>
	 (lambda (path)
	   (load-shared-object path))]
	[else
	 (load-shared-object "socket.so")])
       ((foreign-procedure "socket_startup" () int))]
      [else (error 'socket "unsupported now: " (machine-type))]))
  
  (define-record-type socket
    (fields [mutable fd]))

  (define socket-guardian
    (make-guardian))

  (define-syntax define-dual
    (syntax-rules ()
      [(_ Macro func [Meta Id Val] ...)
       (begin
	 (define-syntax (Macro stx)
	   (define (convert Name*)
	     (apply func
		    (map (lambda (f)
			   (case f
			    [(Meta) Val] ...
			    [else (syntax-violation 'Macro "invalid Identifier: " f)]))
			 Name*)))
	   (syntax-case stx ()
	     [(k Name (... ...))
	      (datum->syntax #'k (convert (syntax->datum #'(Name (... ...)))))]))
	 (define Id Val) ...)]))

  (meta-cond
   [(eq? (machine-type) 'ti3nt)
    (define-dual address-family (lambda (a) a)
      (unspec *af-unspec* 0)
      (inet *af-inet* 2)
      (inet6 *af-inet6* 23))

    (define-dual socket-domain (lambda (a) a)
      (stream *sock-stream* 1)
      (datagram *sock-dgram* 2))

    (define-dual address-info bitwise-ior
      (canoname *ai-canonname* #x2)
      (numerichost *ai-numerichost* #x4)
      (v4mapped *ai-v4mapped* #x800)
      (all *ai-all* #x100)
      (addrconfig *ai-addrconfig* #x400))

    (define-dual ip-protocol (lambda (a) a)
      (ip *ipproto-ip* 0)
      (tcp *ipproto-tcp* 6)
      (udp *ipproto-udp* 17))

    (define-dual message-type bitwise-ior
      (none *msg-none* 0)
      (peek *msg-peek* 2)
      (oob *msg-oob* 1)
      (wait-all *msg-waitall* 8))

    (define-dual shutdown-method (lambda x
				   (if (and (memq 0 x) (memq 1 x))
				       2
				       (apply bitwise-ior x)))
      (read *shut-rd* 0)
      (write *shut-wr* 1)
      (both *shut-rdwr* 2))])

  (define socket-merge-flags bitwise-ior)
  (define (socket-purge-flags base . flag*)
    (apply bitwise-and base (map bitwise-not flag*)))

  (define make-client-socket
    (let ([proc (foreign-procedure "make_client_socket"
				   (string string int int int int) int)])
      (lambda+
       (node service [ai-family *af-inet*] [ai-socktype *sock-stream*]
	     [ai-flags (socket-merge-flags *ai-v4mapped* *ai-addrconfig*)]
	     [ai-protocol *ipproto-ip*])
       (define fd (proc node service ai-family ai-socktype ai-flags ai-protocol))
       (define sock
	 (if (< fd 0)
	     (error 'make-client-socket "connect fail" fd)
	     (make-socket fd)))
       (socket-guardian sock)
       sock)))

  (define make-server-socket
    (let ([proc (foreign-procedure "make_server_socket" (string int int int) int)])
      (lambda+
       (service [ai-family *af-inet*] [ai-socktype *sock-stream*]
		[ai-protocol *ipproto-ip*])
       (define fd (proc service ai-family ai-socktype ai-protocol))
       (define sock
	 (if (<= fd 0)
	     (error 'make-server-socket "fail" fd)
	     (make-socket fd)))
       (socket-guardian sock)
       sock)))

  (define (verify-fd who sock)
    (define fd
      (if (socket? sock)
	  (socket-fd sock)
	  (error who "not a socket" sock)))
    (if (<= fd 0)
	(error who "closed fd")
	fd))

  (define socket-accept
    (let ([proc (foreign-procedure "socket_accept" (int) int)])
      (lambda (sock)
	(define fd (verify-fd 'socket-accept sock))
	(let ([accepted (proc fd)])
	  (when (= accepted -1)
	    (error 'socket-accept "invalid socket"))
	  (let ([sock (make-socket accepted)])
	    (socket-guardian sock)
	    sock)))))

  (define-values (socket-send socket-send!)
    (let ([proc (foreign-procedure "socket_send" (int ptr size_t size_t int) int)])
      (values
       (lambda+
	(sock bv [flags 0])      
	(proc (verify-fd 'socket-send sock) bv 0 (bytevector-length bv) flags))
       (lambda+
	(sock bv [offset 0] [length #f] [flags 0])
	(define len
	  (or length
	      (- (bytevector-length bv) offset)))
	(proc (verify-fd 'socket-send! sock) bv offset length flags)))))

  (define-values (socket-recv socket-recv!)
    (let ([proc (foreign-procedure "socket_recv" (int ptr size_t size_t int) int)])
      (values
       (lambda+
	(sock size [flags 0])
	(define buf (make-bytevector size))
	(define n (proc (verify-fd 'socket-recv sock) buf 0 size flags))
	(cond
	 [(= n size) buf]
	 [(> n 0) (let ([bv (make-bytevector n)])
		    (bytevector-copy! buf 0 bv 0 n)
		    bv)]
	 [else (bytevector)]))
       (lambda+
	(sock bv [offset 0] [length #f] [flags 0])
	(define len
	  (or length
	      (- (bytevector-length bv) offset)))
	(proc (verify-fd 'socker-recv! sock) bv offset length flags)))))

  (define socket-shutdown
    (let ([proc (foreign-procedure "socket_shutdown" (int int) int)])
      (lambda (sock how)
	(proc (verify-fd 'socket-shutdown sock) how))))

  (define socket-close
    (let ([proc (foreign-procedure "socket_close" (int) int)])
      (lambda (sock)
	(proc (verify-fd 'socket-close sock))
	(socket-fd-set! sock -1))))

  (define (socket-input-port sock)
    (make-custom-binary-input-port
     "socket"
     (lambda (bv start n)
       (socket-recv! sock bv start n))
     #f #f #f))

  (define (socket-output-port sock)
    (make-custom-binary-output-port
     "socket"
     (lambda (bv start n)
       (socket-send! sock bv start n))
     #f #f #f))

  (define (call-with-socket sock proc)
    (with-exception-handler
	(lambda (e)
	  (socket-close sock)
	  (raise e))
      (let ([ret (proc sock)])
	(socket-close sock)
	ret)))
  )
