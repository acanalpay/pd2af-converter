#lang racket

(provide (all-defined-out))

(define settings
	(hash
		; 'root-url "http://pd2af.me"
		;'root-url "http://188.166.159.222"
		'root-url (getenv "ROOT_URL")
		'paths
			(hash
				;;; 'inwin-win7 "../../../../../../../../denis_core/projects/pd2af/html/generated"
				; 'inwin-win7 "../../../../../../../../denis_core/projects/sbgn-pd2af/server/static/generated"
				'digitalocean "/server/pd2af/html/generated"
				; 'pd2af.me "/server/pd2af/static/generated"
				; 'pd2af.org "/home/jpellet/pd2af/server/static/generated"
				'lcsb (build-path (getenv "SRV_ROOT") "html" "generated")
			)
	)
)

(define (get-settings . path)
	(letrec ((get-settings-rec
								(λ (settings pathlst)
										(cond
											((empty? pathlst) settings)
											(else (get-settings-rec (hash-ref settings (car pathlst)) (cdr pathlst)))))))
		(get-settings-rec settings path)))


