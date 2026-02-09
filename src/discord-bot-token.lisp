(in-package #:discord-bot-token)


;;;; ------------------------------------------------------------------
;;;; botトークンの取得 ------------------------------------------------
;;;; ------------------------------------------------------------------


(defparameter *token-filepath* "~/common-lisp/discord-bot/data/token.txt")
(defparameter *bot-token* nil)

(defun set-bot-token ()
  (setf *bot-token* (with-open-file (token (pathname *token-filepath*)
					   :direction :input)
		      (read-line token))))





