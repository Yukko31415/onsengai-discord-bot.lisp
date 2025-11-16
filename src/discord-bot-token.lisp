(in-package #:discord-bot-token)


;;;; ------------------------------------------------------------------
;;;; botトークンの取得 ------------------------------------------------
;;;; ------------------------------------------------------------------


(defparameter *token-filepath* "~/common-lisp/discord-bot/data/token.txt")

(defun set-bot-token (filepath)
  (defparameter *bot-token* 
    (with-open-file (token (pathname filepath)
			   :direction :input)
      (read-line token))))

(defcommand :token :initialize arg
  (set-bot-token *token-filepath*))



