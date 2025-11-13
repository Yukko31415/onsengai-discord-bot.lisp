

(in-package #:discord-bot-post)




;;;; ------------------------------------------------------------------
;;;; Discord API連携
;;;; ------------------------------------------------------------------


(defun send-discord-message (embed-data)
  "Discordの指定されたチャンネルにEmbedメッセージを送信する"
  (let* ((url (format nil "https://discord.com/api/v10/channels/~A/messages" *channel-id*))
	 (headers `(("Authorization" . ,(format nil "Bot ~A" *bot-token*))
		    ("Content-Type" . "application/json")))

	 ;; 1. 空のハッシュテーブルを作成
	 (payload-ht (make-hash-table :test 'equal))
	 ;; 2. "embeds"というキーで、embed-dataを要素に持つリストを値として設定
	 (payload (progn
		    (setf (gethash "embeds" payload-ht) (list embed-data))
		    ;; 3. ハッシュテーブルをJSON文字列にエンコード
		    (cl-json:encode-json-to-string payload-ht))))
    
    (handler-case
	(drakma:http-request url
			     :method :post
			     :additional-headers headers
			     :content payload
			     :content-type "application/json")
      (error (e)
	(format t "Discord送信エラー: ~A~%" e)))))
