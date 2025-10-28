




;;;; --------------------------------------------------------------
;;;; ジェネリック関数定義  ----------------------------------------


(defpackage #:discord-bot-define-generic
  (:use #:cl)

  (:export #:bot-command))



;;;; --------------------------------------------------------------
;;;; 環境変数定義  ------------------------------------------------


(defpackage #:discord-bot-preloads
  (:use #:cl
	#:lparallel.queue
	#:discord-bot-define-generic)

  (:export #:*bot-token*
	   #:*seen-items*
	   #:*key-queue*))


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
;;;; コマンド管理  ------------------------------------------------


(defpackage #:discord-bot-command-handler
  (:use #:cl
	#:lparallel.queue)

  (:export #:add-command
	   #:clear-command
	   #:get-command))


;;;; --------------------------------------------------------------
;;;; rss処理  -----------------------------------------------------


(defpackage #:discord-bot-rss
  (:use #:cl
	#:bordeaux-threads
	#:lparallel.queue
	#:discord-bot-preloads
	#:discord-bot-define-generic
	#:discord-bot-define-class)

  (:import-from #:discord-bot-command-handler
		#:add-command)

  (:export #:start-rss-bot))


;;;; --------------------------------------------------------------
;;;; メインループ  ------------------------------------------------


(defpackage #:discord-bot-main
  (:use #:cl
	#:slynk
	#:bordeaux-threads
	#:discord-bot-command-handler
	#:discord-bot-rss
	#:discord-bot-define-generic)

  (:export #:start
	   #:end))

