

(in-package #:discord-bot-post)



;;;; ------------------------------------------------------------------
;;;; Discord API連携
;;;; ------------------------------------------------------------------


(defun make-header (bot-token content-type)
  "bot-tokenとcontent-typeを受け取り、ヘッダーのリストを生成して返す"
  (list `(#:Authorization . ,(format nil "Bot ~A" bot-token))
	`(#:Content-Type . ,content-type)))



(defun make-content (content)
  `(:|content| ,content))



(defun keyword-downcase (keyword)
  "キーワードを受け取り、小文字化した新しいキーワードを返す"
  (intern (string-downcase (symbol-name keyword)) :keyword))

<<<<<<< HEAD


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
=======
(defun make-plist-for-embeds (plist)
  (loop :for (key content) :on plist :by #'cddr 
	:append (list (keyword-downcase key) content)))
>>>>>>> sub



(defun embed (&rest arg &key title description url timestamp color author footer)
  (declare (ignore title description url timestamp color author footer))
  (declare ((or simple-string null) title description url timestampxs))
  (declare ((or fixnum null) color))
  (declare (list footer author))
  (make-plist-for-embeds arg))

(defun footer (text &rest arg &key icon_url proxy_icon_url)
  (declare (ignore icon_url proxy_icon_url))
  (declare (simple-string text))
  (declare ((or simple-string null) icon_url proxy_icon_url))
  (append `(:|text| ,text) (make-plist-for-embeds arg)))

(defun author (name &rest arg &key url icon_url proxy_icon_url)
  (declare (ignore url icon_url proxy_icon_url))
  (declare (simple-string  name))
  (declare ((or simple-string null) url icon_url proxy_icon_url))
  (append `(:|name| ,name) (make-plist-for-embeds arg)))


(defun make-embeds (content)
  `(:|embeds| ,content))

(defun make-payload (content embeds)
  (append (make-content (or content ""))
	  (make-embeds embeds)))





(defun %send-discord-message (channel-id bot-token &key content embeds)
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"

  (let ((url (format nil "https://discord.com/api/v10/channels/~A/messages" channel-id))
	(headers (make-header bot-token "application/json"))
	(payload (jonathan:to-json (make-payload content embeds))))

    (handler-case
	(drakma:http-request url
			     :method :post
			     :additional-headers headers
			     :content payload
			     :external-format-out :utf-8)
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))

    payload))

(defmacro send-discord-message (channel-id bot-token &key content embeds)
  (let ((list (cons 'list embeds)))
    `(%send-discord-message ,channel-id ,bot-token :content ,content :embeds ,list)))








