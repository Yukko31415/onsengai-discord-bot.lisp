
;;;; --------------------------------------------------------------
;;;; ジェネリック関数定義  ----------------------------------------


(defpackage #:discord-bot-define-generic
  (:use #:cl)

  (:export #:run-command
	   #:defcommand))



;;;; --------------------------------------------------------------
;;;; rss処理  -----------------------------------------------------


(defpackage #:discord-bot-rss
  (:use #:cl
	#:bordeaux-threads
	#:lparallel.queue
	
	#:discord-bot-define-generic)

  (:export #:run-rss-bot))


;;;; --------------------------------------------------------------
;;;; メインループ  ------------------------------------------------


(defpackage #:discord-bot-main
  (:use #:cl
	#:slynk
	#:bordeaux-threads
	
	#:discord-bot-rss
	#:discord-bot-define-generic)

  (:export #:start
	   #:end))
