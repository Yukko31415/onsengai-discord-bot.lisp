
(in-package #:discord-bot-rss)


;;;; --------------------------------------------------------------
;;;; rss-bot-commands ---------------------------------------------


(defcommand :rss-post (object cons)
  (dolist (i object)
    (bot-command key i)))

(defcommand :rss-post (object sandbox)
	   (check-rss-feeds
	    (link object)
	    (color object)
	    (icon object)))

(defcommand :rss-post (object wikidot-jp)
	   (check-rss-feeds
	    (link object)
	    (color object)
	    (icon object)))

(defcommand :save-rss-queue arg
  (output-key-plist-to-data *key-queue*))





;;;; ---------------------------------------------------------------------------------
;;;; botトークンの取得 ---------------------------------------------------------------


(defvar *bot-token*
  (with-open-file (token (pathname "~/common-lisp/discord-bot/data/token.txt")
			 :direction :input)
    (read-line token))
  "Discordボットのトークン")


;;;; ---------------------------------------------------------------------------------
;;;; *seen-items*の初期設定 ----------------------------------------------------------




(defparameter *filepath* "~/common-lisp/discord-bot/data/rss-queue-list.txt")


(defun set-seen-items (pathname)
  (with-open-file (content pathname
			   :direction :input)
		  (let ((list (read content)))
		    (let ((hash-table (make-hash-table :test 'equal))
			  (keys-list list))
		      (dolist (key keys-list)
			(setf (gethash key hash-table) t))
		      hash-table))))

(defun set-key-queue (pathname)
  (with-open-file (content pathname
			   :direction :input)
		  (let ((list (read content)))
		    (let ((keys-list list)
			  (queue (make-queue)))
		      (dolist (i keys-list)
			(push-queue i queue))
		      queue))))





;; RSSで閲覧したページのリンクを保存するキー
(defparameter *seen-items* nil  "投稿済みの記事を記録するハッシュテーブル")

;; キーを追加した順番に管理するリスト（キューとして使用）

(defparameter *key-queue* nil)



(defun initialize ()
  (setf *seen-items* (set-seen-items *filepath*))
  (setf *key-queue* (set-key-queue *filepath*)))


;;;; ------------------------------------------------------------------
;;;; グローバル変数と設定
;;;; ------------------------------------------------------------------


(defparameter *channel-id* "1121439803213365279" "投稿先のDiscordチャンネルID")


(defvar *check-interval* 300 "RSSフィードのチェック間隔（秒）")

;;; ボット状態管理


(defvar *bot-running* nil "ボットが実行中かどうかのフラグ")
;; データを何個まで保持するかの上限


(defparameter *max-items* 100)

(defparameter *sandbox-rss-link*
  (make-instance 'sandbox
		 :link "http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml"))


(defparameter *wikidot-jp-rss-link*
  (make-instance 'wikidot-jp
		 :link "http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml"))




;;;; ------------------------------------------------------------------
;;;; XMLデータの抽出
;;;; ------------------------------------------------------------------


(defun find-child-node (parent-node child-name)
  "親ノードの子から指定された名前の最初の子ノードを見つける"
  (find-if (lambda (node)
             (and (typep node 'xmls:node)
                  (string-equal (xmls:node-name node) child-name)))
           (xmls:node-children parent-node)))


(defun extract-matched-string (html-string)
  "HTML文字列から「付与予定タグ:」で始まる行を正確に一行だけ抽出する"
  (when (and html-string (> (length html-string) 0))
    (let* ((parsed-html (lquery:$ (initialize html-string)))
           ;; 1. まず<p>タグと<strong>タグをすべて候補としてリストアップ
           (candidate-nodes (coerce (lquery:$ parsed-html "p, strong") 'list)))

      ;; 2. 候補の中から、目的のテキストを含む最初のノードを見つける
      (let ((target-node (find-if
                          (lambda (node)
                            (let ((node-text (format nil "~{~A~^ ~}"
                                                     (coerce (lquery:$ node (text)) 'list))))
                              (search "付与予定タグ:" node-text)))
                          candidate-nodes)))
        (when target-node
          ;; 3. 見つかったノードのテキストを改行で分割
          (let* ((full-text (format nil "~{~A~^ ~}" (coerce (lquery:$ target-node (text)) 'list)))
                 (lines (str:lines full-text)))
            ;; 4. 分割した行の中から「付与予定タグ:」で始まる行を探して返す
            (find-if (lambda (line)
                       (str:starts-with-p "付与予定タグ:" (str:trim line)))
                     lines)))))))



(defun extract-inpagetags-text (html-string)
  "HTML文字列から.inpagetagsクラス内のテキストを抽出し、「付与予定タグ: 」を付けて返す"
  (when (and html-string (> (length html-string) 0))
    (let* ((parsed-html (lquery:$ (initialize html-string)))
           ;; .inpagetagsクラス内のspan要素を直接指定
           (tags-node (lquery:$ parsed-html ".inpagetags span")))
      ;; ノードが見つかった場合のみ処理を続行
      (when (and tags-node (> (length (coerce tags-node 'list)) 0))
        ;; 最初のspanノードのテキストを取得し、前後の空白を削除
        (let ((tags-text (str:trim (plump:text (aref tags-node 0)))))
          ;; テキストが空でないことを確認
          (when (> (length tags-text) 0)
            (format nil "付与予定タグ: ~a" tags-text)))))))



(defun get-tags-line (html-string)
  "HTMLからタグ行を抽出する。
   まず「付与予定タグ:」で始まる行を探し（extract-matched-string）、
   見つからなければ.inpagetagsクラスからの抽出を試みる。"
  ;; orは最初のNILでない値を返す。両方見つからなければNILを返す。
  (or (extract-matched-string html-string)
      (extract-inpagetags-text html-string)))


(defun get-node-text (node)
  "ノードの持つすべての子要素から文字列を連結して返す"
  (when node
    (with-output-to-string (s)
      (map nil
	   (lambda (child)
	     (when (stringp child)
	       (write-string child s)))
	   (xmls:node-children node)))))


(defun parse-item (item-node)
  "<item>または<entry>ノード(構造体)から情報を抽出する
   最終的にplistを作成して返す"
  (let* ((title (get-node-text (find-child-node item-node "title")))
	 (link (get-node-text (find-child-node item-node "link")))
	 (pub-date (get-node-text (find-child-node item-node "pubDate")))
	 (description-html (get-node-text (or (find-child-node item-node "content:encoded")
					      (find-child-node item-node "description")))))

    (list :title title
	  :link link
	  :description (get-tags-line description-html)
	  :pub-date pub-date)))



(defun extract-items (parsed-root-node)
  "パースされたXMLのルートノードから<item>要素のリストを抽出し、
   plistにして返す"
  (let* ((channel-node (find-child-node parsed-root-node "channel"))
         (items '()))
    (when channel-node
      (map nil
           (lambda (node)
             (when (and (typep node 'xmls:node)
                        (string-equal (xmls:node-name node) "item"))
               (push (parse-item node) items)))
           (xmls:node-children channel-node)))
    items))



;;;; ------------------------------------------------------------------
;;;; RSSフィードの取得と解析
;;;; ------------------------------------------------------------------



(defun get-rss-source (url)
  "URLを受け取り、問い合わせてRSSを受け取る"
  (drakma:http-request
   url			       ; URLは文字列またはPURI:URIオブジェクト
   :force-binary t
   :user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36"
   :additional-headers '(("Accept" . "application/xml, text/xml, */*")
			 ("Accept-Language" . "ja,en-US;q=0.9,en;q=0.8"))
   :redirect t))


(defun debug-output (xml-string charset)
  "XMLファイルをdebug-output.xmlに書き出す（デバッグ用）"
  (with-open-file (stream "~/common-lisp/discord-bot/debug/debug-output.xml" ; 拡張子を.xmlに
			  :direction :output
			  :if-exists :supersede
			  :external-format charset) ; 文字コードを指定
    (write-string xml-string stream)))


(defun decompress-gzip (headers response-body)
  "もし受け取ったRSSファイルがgzipで圧縮されていた場合、それを解凍する"
  (if (string-equal (drakma:header-value :content-encoding headers) "gzip")
      (progn
	(chipz:decompress nil 'chipz:gzip response-body))
      response-body))


(defun debug-parse-rss (url)
  "指定されたURLからRSSフィードを取得・パースする。Gzip圧縮とURLエンコーディングに対応"

  (format t "~%--- [詳細デバッグ] ~A の解析を開始 ---~%" url)
  
  (handler-case
      (multiple-value-bind (response-body status-code headers) (get-rss-source url)
	
	(format t "[詳細デバッグ] サーバーからのステータスコード: ~A~%" status-code)
	
	(when (= status-code 200)
	  ;; Gzipで圧縮されているかチェックし、必要なら伸長する
	  (let* ((body-octets (decompress-gzip headers response-body))
		 (charset (or (drakma:header-value :charset headers) :utf-8))
		 (xml-string (babel:octets-to-string body-octets :encoding charset)))
	    
	    ;; デバッグ用に、人間が読める伸長済みのXMLをファイルに書き出す
	    (debug-output xml-string charset)
	    (format t "[詳細デバッグ] ★重要★ 伸長後の応答を 'debug-output.xml' に保存しました。~%")

	    ;; パースして、plistにして返す
	    (format t "[詳細デバッグ] XMLのパースを試みます...~%")
	    (let ((parsed (xmls:parse xml-string)))
	      (format t "[詳細デバッグ] XMLパース成功！~%")
	      (extract-items parsed)))))
    
    (error (e)
      (format t "[詳細デバッグ] ★★★ エラー発生 ★★★: ~A~%" e)
      nil)))





;;;; ------------------------------------------------------------------
;;;; Discord API連携とメインループ
;;;; ------------------------------------------------------------------

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





;;;; ------------------------------------------------------------------
;;;; メインループ
;;;; ------------------------------------------------------------------



(defun format-rss-message (item color icon)
  "RSSアイテムをDiscordのEmbedデータ（連想リスト）に変換する"
  `(("title"       . ,(format nil "~a ~a" icon (getf item :title)))
    ("description" . ,(let ((desc (getf item :description)))
                        ;; descriptionが長すぎる場合に400文字で切り詰める
                        (if (> (length desc) 400)
                            (concatenate 'string (subseq desc 0 400) "...")
                            desc)))
    ("url"         . ,(getf item :link))
    ("color"       . ,color) ; 色を16進数の #3498DB (青色) から10進数に変換したもの
    ("timestamp"   . ,(local-time:format-timestring nil (local-time:now))) ; 現在時刻のタイムスタンプ
    ("footer"      . (("text" . "RSS Feed Bot")))))


(defun add-data (key value)
  "新しいデータを追加し、上限を超えていれば最も古いデータを削除する"
  ;; --- ステップ1: 新しいデータを追加 ---
  (setf (gethash key *seen-items*) value)
  ;; 新しいキーをキューの末尾に追加
  (push-queue key *key-queue*)

  ;; --- ステップ2: 上限を超えているかチェック ---
  (loop
    #:do (if (> (queue-count *key-queue*) *max-items*)
	     ;; --- ステップ3: 超えていれば、一番古いデータを削除 ---
	     ;; キューの先頭から一番古いキーを取得
	     (let ((oldest-key (pop-queue *key-queue*)))
	       ;; ハッシュテーブルからそのキーのデータを削除
	       (remhash oldest-key *seen-items*)
	       (format t "--- 上限を超えたため、一番古いキー「~A」を削除しました ---~%" oldest-key))
	     (return))))



(defun item-check-and-send (items color icon)
  (progn
    (format t "[デバッグ] ~D件の記事が見つかりました。~%" (length items))
    (dolist (item items)
      (let ((item-id (getf item :link))
	    (title (getf item :title)))

	(format t "[デバッグ] 記事チェック: ~A~%" title)

	(unless (gethash item-id *seen-items*)
	  (format t "[デバッグ] >> 新着記事です！ Discordに投稿します。~%")
	  (add-data item-id t)
	  (send-discord-message (format-rss-message item color icon))
	  (sleep 1))))))



(defun check-rss-feeds (url color icon)
  "登録されたすべてのRSSフィードをチェックし、新しい項目があれば投稿する"
  (format t "~%~&[デバッグ] RSSフィードのチェックを開始します...")
  (format t "[デバッグ] チェック中: ~A~%" url)
  (let ((items (debug-parse-rss url)))
    (if (null items)
	(format t "[デバッグ] このURLからは記事を取得できませんでした、または空でした。~%")
      (item-check-and-send items color icon))))




(defun output-key-plist-to-data (queue)
  "現在のキューの中身を書き出す"
  (format t "~%[ログ保存] 現在のキューをファイルに書き出します~%")
  (with-open-file (out "~/common-lisp/discord-bot/data/rss-queue-list.txt"
		       :direction :output
		       :if-exists :supersede)
    (let* ((queue-head (lparallel.raw-queue::head
			(lparallel.cons-queue::%%%%.cons-queue.impl queue)))
	   (queue-tail (lparallel.raw-queue::tail
			(lparallel.cons-queue::%%%%.cons-queue.impl queue)))
	   (copied-queue (append queue-head queue-tail)))
      (format out "~s" copied-queue))))









;;;; --------------------------------------------------------------
;;;; rss-command-list ---------------------------------------------


(defun start-rss-bot ()
  (add-command (:loop-command
		(:command-dotimes 5
				  (:rss-post *sandbox-rss-link*
					     *wikidot-jp-rss-link*)
				  (:sleep 300))
		(:save-rss-queue))))

