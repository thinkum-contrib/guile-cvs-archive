#! /usr/bin/guile -s
!#

; The socket' name
(define SOCKET-1 "/tmp/socket1")

; Connect to the remote socket as client
(define  sock (socket PF_UNIX SOCK_STREAM 0))
(connect sock AF_UNIX SOCKET-1)

; Send something
(send sock "jiba")
#! /usr/bin/guile -s
!#

; Use threads
(use-modules (ice-9 threads))

; The socket' names
(define SOCKET-1 "/tmp/socket1")

; Remove socket if it exists
(catch #t
       (lambda () (delete-file SOCKET-1))
       (lambda args #f)
       )

; Creates the (UNIX) socket
(define sock (socket PF_UNIX SOCK_STREAM 0))
(bind   sock AF_UNIX SOCKET-1)
(listen sock 3)

; The function that will do something with the connection
(define (process-connection s)
  (display s) (newline)

  ; Read the first character in the socket and display it
  (let ((str (make-string 1)))
    (recv! s str)

    ; This will never print !
    (display "I've read : ") (display str) (display "\n")
    )
  )

; Call "accept", get the socket and pass it to "process-connection"
; in a new thread.
(while #t; If you comment this line, the bug no longer occurs !
       (let ((s (car(accept sock))))
         (display "connection...\n")
         (make-thread (lambda () (process-connection s)))
         )
       ); Comment this too !
