
;;;; --------------------------------------------------------------
;;;; ジェネリック関数定義
;;;; --------------------------------------------------------------

(defpackage #:discord-bot-define-generic
  (:documentation "generic関数bot-commandを定義する")
  (:use #:cl)

  (:export #:run-command
	   #:defcommand))

;;;; --------------------------------------------------------------
;;;; トークン管理
;;;; --------------------------------------------------------------

(defpackage #:discord-bot-token
  (:documentation "botのトークンを管理する")
  (:use #:cl
	#:discord-bot-define-generic)

  (:export #:*bot-token*))

;;;; --------------------------------------------------------------
;;;; discordポスト処理
;;;; --------------------------------------------------------------

(defpackage #:discord-bot-post
  (:documentation "DiscordにJSONをポストする")
  (:use #:cl
	;; dexadorを使う予定だけど、
	;; delete関数でclとコンフリクトするのでuseはしない

	#:discord-bot-define-generic
	#:discord-bot-token
	))



;;;; --------------------------------------------------------------
;;;; rss処理
;;;; --------------------------------------------------------------

(defpackage #:discord-bot-rss
  (:documentation "rss-botのメインロジック")
  (:use #:cl
	#:bordeaux-threads
	#:lparallel.queue
	
	#:discord-bot-define-generic
	#:discord-bot-post)

  (:export #:run-rss-bot))


;;;; --------------------------------------------------------------
;;;; メインループ
;;;; --------------------------------------------------------------

(defpackage #:discord-bot-main
  (:documentation "エントリーポイントやイニシャライズの管理")
  (:use #:cl
	#:slynk
	#:bordeaux-threads
	
	#:discord-bot-rss
	#:discord-bot-define-generic)

  (:export #:start
	   #:end))
