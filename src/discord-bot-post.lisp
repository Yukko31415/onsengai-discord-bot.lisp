

(in-package #:discord-bot-post)



;;;; ------------------------------------------------------------------
;;;; Discord API連携
;;;; ------------------------------------------------------------------

(defun get-bot-token ()
 (with-open-file (token (pathname "~/common-lisp/discord-bot/data/token.txt")
			   :direction :input)
   (read-line token)))


(defun make-header (bot-token content-type)
  "bot-tokenとcontent-typeを受け取り、ヘッダーのリストを生成して返す"
  (list `(#:Authorization . ,(format nil "Bot ~A" bot-token))
	`(#:Content-Type . ,content-type)))




(defgeneric %make-content (content)
  (:documentation "文字列、リストを受け取ってペイロードを返す"))

(defmethod %make-content ((content string))
  `(:|content| ,content))

(defmethod %make-content ((content number))
  `(:|content| ,(write-to-string content)))



(defun keyword-downcase (k)
  "キーワードを受け取り、小文字化した新しいキーワードを返す"
  (intern (string-downcase (symbol-name k)) :keyword))

(defun make-embeds (plist)
  "carにあるkeywordを小文字にする"
  (when plist
    (let ((x (car plist)))
      (cons (if (keywordp x) (keyword-downcase x) x)
	    (make-embeds (cdr plist))))))

(defmethod %make-content ((content cons))
  (let ((plists (mapcar #'make-embeds content)))
    
    (log:info "plists is ~s~%" plists)
    
    `(:|embeds| ,plists)))



(defun make-content (content)
  "型を受け取って文字列や埋め込みに変換する"
  (typecase content
    ((or number string)
     (%make-content content))
    (cons
     (append (%make-content (first content))
	     (%make-content (rest content))))))




(defun send-discord-message (channel-id content bot-token)
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"
  
  (let* ((url (format nil "https://discord.com/api/v10/channels/~A/messages" channel-id))
	 (headers (make-header bot-token "application/json"))
	 (payload (jonathan:to-json (make-content content))))

    (log:info "the sent message is: ~a~%" payload)
    
    (handler-case
	(dex:post url
		  :headers headers
		  :content payload)
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))
    
    payload))



;;;; ------------------------------------------------------------------
;;;; defcommand
;;;; ------------------------------------------------------------------


(defcommand :post :post-message arg 
  (let ((channel-id (first arg))
	(content (second arg)))
    (send-discord-message channel-id content *bot-token*)))


;; sample
;; (run-command (:post :post-message 1406525194289287311
;; 		    ("hi" (:title "title"
;; 			   :description "a"))))

