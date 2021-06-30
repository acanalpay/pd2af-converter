#lang racket

(require web-server/servlet web-server/servlet-env)
(require "../server/servlets/translator-servlet.rkt")
;
; make background process: 'sudo -b nohup racket start-server.rkt'

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

(serve/servlet
	translator
	#:listen-ip #f ; #f - open to external http requests
	#:port port
	; #:servlet-regexp #rx"" ; capture all top-level requests
	#:servlet-regexp #rx"/translate"
	#:extra-files-paths (list (build-path (getenv "SRV_ROOT") "html"))
	#:launch-browser? #f
	#:log-file (getenv "LOGFILE")
)
