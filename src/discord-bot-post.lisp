

(in-package #:discord-bot-post)



;;;; ------------------------------------------------------------------
;;;; Discord API連携 -------------------------------------------------
;;;; ------------------------------------------------------------------

(defun get-bot-token ()
 (with-open-file (token (pathname "~/common-lisp/discord-bot/data/token.txt")
			   :direction :input)
   (read-line token)))


(defun make-header (bot-token content-type)
  "bot-tokenとcontent-typeを受け取り、ヘッダーのリストを生成して返す"
  (list `(#:Authorization . ,(format nil "Bot ~A" bot-token))
	`(#:Content-Type . ,content-type)))


(defun make-plist (keyword prop)
  "propがnilならばnilを返し、tならばキーワードとのplistにして返す"
  (when prop
    `(,keyword ,prop)))


(defgeneric make-content (content)
  (:documentation "文字列、リストを受け取ってペイロードを返す"))

(defmethod make-content :around (content)
  (typecase content
    (cons
     (let ((first-content (car content)))
       (typecase first-content
	 (string
	  (append
	   (apply #'make-content `(,first-content))
	   (apply #'make-content (cdr content))))
	 (cons
	  (call-next-method)))))
    (t
     (call-next-method))))


(defmethod make-content ((content string))
  `(:content ,content))

(defmethod make-content ((content number))
  `(:content ,(write-to-string content)))

(defmethod make-content ((content cons))
  `(:embeds ,(list content)))


(defun send-discord-message (channel-id content bot-token)
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"
  
  (let* ((url (format nil "https://discord.com/api/v10/channels/~A/messages" channel-id))
	 (headers (make-header bot-token "application/json"))
	 (payload (cl-json:encode-json-plist-to-string (make-content content))))
    
    (handler-case
	(dex:post url
		  :headers headers
		  :content payload)
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))))


;;;; ------------------------------------------------------------------
;;;; defcommand -------------------------------------------------------
;;;; ------------------------------------------------------------------

(defcommand :post :post-message arg
  (let ((channel-id (first arg))
	(content (second arg)))
    (send-discord-message channel-id content *bot-token*)))



;; こんな風に送信する
;; (run-command (:post :post-message 1406525194289287311
;; 		    ((:title . "hi")
;; 		     (:description . "これは内容です"))))

