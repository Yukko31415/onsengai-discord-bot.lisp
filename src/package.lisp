;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name




;;
;; token

(defpackage #:discord-bot-token
  (:documentation "botのトークンを管理する")
  (:use #:cl)

  (:export #:*bot-token*
	   #:set-bot-token))


;;
;; post

(defpackage #:discord-bot-post
  (:documentation "DiscordにJSONをポストする")
  (:use #:cl
	#:discord-bot-token)
  (:export #:send-discord-message
	   #:embed
	   #:footer
	   #:author))

;;
;; rss

(defpackage #:discord-bot-rss
  (:documentation "rss-botのメインロジック")
  (:use #:cl
	#:discord-bot-post)

  (:export #:run
	   #:stop
	   #:make-rss-bot))


;;
;; main


(defpackage #:discord-bot
  (:use #:cl)
  (:documentation "The discord-bot package.")
  (:export #:main))
