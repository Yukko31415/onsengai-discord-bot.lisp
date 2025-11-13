

(in-package #:discord-bot-post)

(defvar *bot-token*
  (with-open-file (token (pathname "~/common-lisp/discord-bot/data/token.txt")
			 :direction :input)
    (read-line token))
  "Discordボットのトークン")

;;;; ------------------------------------------------------------------
;;;; Discord API連携
;;;; ------------------------------------------------------------------


(defun make-header (bot-token content-type)
  "bot-tokenとcontent-typeを受け取り、ヘッダーのリストを生成して返す"
  (list `(#:Authorization . ,(format nil "Bot ~A" bot-token))
	`(#:Content-Type . ,content-type)))


(defun send-discord-message (embed-data channel-id bot-token)
  
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"
  
  (let* ((url (format nil "https://discord.com/api/v10/channels/~A/messages" channel-id))
	 
	 (headers (make-header bot-token "application/json"))

	 ;; 1. 空のハッシュテーブルを作成
	 (payload-ht (make-hash-table :test 'equal))
	 ;; 2. "embeds"というキーで、embed-dataを要素に持つリストを値として設定
	 (payload (progn
		    (setf (gethash "embeds" payload-ht) (list embed-data))
		    ;; 3. ハッシュテーブルをJSON文字列にエンコード
		    (cl-json:encode-json-to-string payload-ht))))

    (handler-case
	(dex:post url
		  :headers headers
		  :content payload)
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))))

(send-discord-message '((#:title . "hello!")) 1406525194289287311 (with-open-file (token (pathname "~/common-lisp/discord-bot/data/token.txt")
			 :direction :input)
    (read-line token)))

