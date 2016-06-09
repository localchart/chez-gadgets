(library (qww6 tcp)
  (export make-tcp-socket tcp-socket? tcp-socket-fd
	  tcp-create-socket tcp-connect tcp-accept tcp-bind tcp-listen
	  tcp-send tcp-recv
	  tcp-set-reuse-addr tcp-set-send-timeout tcp-set-recv-timeout
	  tcp-shutdown tcp-close
	  tcp-clear-sockets)
  (import (chezscheme))

  (define-record tcp-socket (fd))
  
  (define dunmmy
    (begin 
      (load-shared-object "ws2_32.dll")
      (if (top-level-bound? 'tcp-dll-path)
	  (load-shared-object (top-level-value 'tcp-dll-path))
	  (load-shared-object "tcp.dll"))))


  (define %tcp-startup
    (foreign-procedure "tcp_startup" () void))
  (define %tcp-connect
    (foreign-procedure "tcp_connect" (int string int) int))
  (define %tcp-set-reuse-addr
    (foreign-procedure "tcp_set_reuse_addr" (int) int))
  (define %tcp-bind
    (foreign-procedure "tcp_bind" (int int) int))
  (define %tcp-listen
    (foreign-procedure "tcp_listen" (int int) int))
  (define %tcp-accept
    (foreign-procedure "tcp_accept" (int) int))
  (define %tcp-send
    (foreign-procedure "tcp_send" (int ptr size_t size_t) int))
  (define %tcp-recv
    (foreign-procedure "tcp_recv" (int ptr size_t size_t) int))
  (define %tcp-set-recv-timeout
    (foreign-procedure "tcp_set_recv_timeout" (int int) int))
  (define %tcp-set-send-timeout
    (foreign-procedure "tcp_set_send_timeout" (int int) int))
  (define %tcp-socket
    (foreign-procedure "tcp_socket" () int))
  (define %tcp-shutdown
    (foreign-procedure "tcp_shutdown" (int) int))
  (define %tcp-close
    (foreign-procedure "tcp_close" (int) int))

  (define-values (tcp-create-socket tcp-accept tcp-clear-sockets)
    (let ([g (make-guardian)])
      
      (define (clear)
	(collect (collect-maximum-generation))
	(let loop ([thing (g)])
	  (when thing
	    (let ([fd (tcp-socket-fd thing)])
	      (unless (= fd -1)
		(%tcp-close fd))
	      (loop (g))))))
      (define (accept sock)
	(define new-fd (make-tcp-socket (%tcp-accept (tcp-socket-fd sock))))
	(g new-fd)
	new-fd)
      (define (tcp)
	(define sock (make-tcp-socket (%tcp-socket)))
	(g sock)
	sock)
      (values tcp accept clear)))

  (define (tcp-connect sock ip port)
    (%tcp-connect (tcp-socket-fd sock) ip port))

  (define (tcp-set-reuse-addr sock)
    (%tcp-set-reuse-addr (tcp-socket-fd sock)))

  (define (tcp-bind sock port)
    (%tcp-bind (tcp-socket-fd sock) port))

  (define (tcp-listen sock backlog)
    (%tcp-listen (tcp-socket-fd sock) backlog))

  (define tcp-send
    (case-lambda
      [(sock bytes)
       (tcp-send sock bytes 0 (bytevector-length bytes))]
      [(sock bytes offset)
       (tcp-send sock bytes offset (- (bytevector-length bytes) offset))]
      [(sock bytes offset len)
       (if (and (bytevector? bytes)
		(>= len 0)
		(<= len (bytevector-length bytes)))
	   (%tcp-send (tcp-socket-fd sock) bytes offset len)
	   (error 'tcp-send "invalid argument"))]))

  (define tcp-recv
    (case-lambda
      [(sock bytes)
       (tcp-recv sock bytes 0 (bytevector-length bytes))]
      [(sock bytes offset)
       (tcp-recv sock bytes offset (- (bytevector-length bytes) offset))]
      [(sock bytes offset len)
       (if (and (bytevector? bytes)
		(>= len 0)
		(<= len (bytevector-length bytes)))
	   (%tcp-recv (tcp-socket-fd sock) bytes offset len)
	   (error 'tcp-recv "invalid argument"))]))

  (define (tcp-set-send-timeout sock ms)
    (%tcp-set-send-timeout (tcp-socket-fd sock) ms))

  (define (tcp-set-recv-timeout sock ms)
    (%tcp-set-recv-timeout (tcp-socket-fd sock) ms))

  (define (tcp-shutdown sock)
    (%tcp-shutdown (tcp-socket-fd sock)))

  (define (tcp-close sock)
    (define ret
      (%tcp-close (tcp-socket-fd sock)))
    (set-tcp-socket-fd! sock -1)
    ret)

  (%tcp-startup)


  )
