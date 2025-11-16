

(in-package #:discord-bot-post)




;;;; ------------------------------------------------------------------
;;;; Discord API連携
;;;; ------------------------------------------------------------------


(defun make-header (bot-token content-type)
  "bot-tokenとcontent-typeを受け取り、ヘッダーのリストを生成して返す"
  (list `(#:Authorization . ,(format nil "Bot ~A" bot-token))
	`(#:Content-Type . ,content-type)))


(defgeneric make-content (content)
  (:documentation "文字列、リストを受け取ってペイロードを返す"))


(defmethod make-content :around (content)
  (typecase content
    (string
     (call-next-method))
    (cons
     (let ((first-content (car content)))
       (typecase first-content
	 (string
	  (append
	   (apply #'make-content `(,first-content))
	   (apply #'make-content (cdr content))))
	 (cons
	  (call-next-method)))))))


(defmethod make-content ((content string))
  `(:content ,content))

(defmethod make-content ((content cons))
  `(:embeds ,(list content)))


(defun send-discord-message (content channel-id bot-token)
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"
  
  (let* ((url (format nil "https://discord.com/api/v10/channels/~A/messages" channel-id))
	 (headers (make-header bot-token "application/json"))
	 (payload (cl-json:encode-json-plist-to-string (make-content content))))

    (format t "~a" payload)
    
    (handler-case
	(dex:post url
		  :headers headers
		  :content payload)
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))))


(send-discord-message '("hi" ((:title . "hi")))
		      1406525194289287311
		      "MTQwNDgzNTk4MDAzNjI3NjI5NQ.GGnSTo.-EVYh64tXgHiZfbnQuBSfLExJEvP4fD85YNFtc")
