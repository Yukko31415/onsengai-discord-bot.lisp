




;;;; --------------------------------------------------------------
;;;; ジェネリック関数定義  ----------------------------------------


(defpackage #:discord-bot-define-generic
  (:use #:cl)

  (:export #:run-command))




;;;; --------------------------------------------------------------
;;;; クラス定義  --------------------------------------------------


(defpackage #:discord-bot-define-class
  (:use #:cl)

  (:export #:sandbox
	   #:sandbox-link
	   #:sandbox-color
	   #:sandbox-icon

	   #:wikidot-jp
	   #:wikidot-jp-link
	   #:wikidot-jp-color
	   #:wikidot-jp-icon))


;;;; --------------------------------------------------------------
;;;; rss処理  -----------------------------------------------------


(defpackage #:discord-bot-rss
  (:use #:cl
	#:bordeaux-threads
	#:lparallel.queue
	#:discord-bot-define-generic
	#:discord-bot-define-class)

  (:import-from #:discord-bot-command-handler
		#:add-command)

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
