(in-package #:discord-bot-rss)


;;;; ------------------------------------------------------------------
;;;; classの設定
;;;; ------------------------------------------------------------------



(defclass feedconfig ()
  ((link-list
    :initarg :url-list :accessor :url-list)
   (color
    :initarg :color :accessor :color)
   (icon
    :initarg :icon :accessor :icon)))


(defclass sandbox (feedconfig)
  ((color
    :initform 49408)
   (icon
    :initform "<:SB:1088712219656728657>")))


(defclass wikidot-jp (feedconfig)
  ((color
    :initform 15007744)
   (icon
    :initform "<:Pos:1088712480865394700>")))




;;;; ------------------------------------------------------------------
;;;; *seen-items*の初期設定
;;;; ------------------------------------------------------------------



(defparameter *queue-list-filepath* "~/common-lisp/discord-bot/data/rss-queue-list.txt")


(defun set-seen-items (pathname)
  "閲覧したページのリンクを保存するハッシュテーブルとキューを作成する"
  (with-open-file (content pathname
			   :direction :input)

    (let* ((hash-table (make-hash-table :test 'equal))
	   (queue (make-queue))
	   (keys-list (read content)))

      (dolist (key keys-list)
	(setf (gethash key hash-table) t)
	(push-queue key queue))

      (defparameter *seen-items* hash-table)
      (defparameter *key-queue* queue))))





;;;; ------------------------------------------------------------------
;;;; グローバル変数と設定
;;;; ------------------------------------------------------------------


(defparameter *channel-id* "1121439803213365279" "投稿先のDiscordチャンネルID")

(defvar *check-interval* 300 "RSSフィードのチェック間隔（秒）")

(defparameter *max-items* 100)

(defparameter *sandbox-rss-link*
  (make-instance 'sandbox
		 :url-list '("http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml")))

(defparameter *wikidot-jp-rss-link*
  (make-instance 'wikidot-jp
		 :url-list '("http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml")))



;;;; ------------------------------------------------------------------
;;;; RSSフィードの取得とパース
;;;; ------------------------------------------------------------------


(defun fetch-and-parse-from-xml (url)
  "リンク先からxmlを取得し、リストにして返す"
  (xmls:parse-to-list (dex:get url)))


(defun find-content-if (key list)
  "受け取ったリストの子ノードを解析し、一致したキーを持つリストを返す"
  (find-if
   #'(lambda (list) (let ((car-list (car list)))
		      (and (consp list)
			   (stringp car-list)
			   (string= car-list key))))
   (cdr list)))


(defun item-member-p (item keys-list)
  "受け取ったアイテムのcarがkeyに一致しているかを判定する"
  (when (consp item)
    (let ((car-item (car item)))
      (member car-item
	      keys-list :test 'equal))))


(defun remove-content-if-not (keys-list item)
  "キーに一致しない子ノードを取り除いて返す"
  (remove-if-not
   #'(lambda (s) (and (consp item)
		      (item-member-p s keys-list)))
   (cdr item)))


;; テストコード

;; (fetch-and-parse-from-xml "http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml")

;; (remove-content-if-not '("item") (find-content-if
;; 			      "channel"
;; 			      (fetch-and-parse-from-xml "http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml")))


;;;; ------------------------------------------------------------------
;;;; feedconfig->item obj
;;;; ------------------------------------------------------------------

(defclass item ()
  ((title
    :initarg :title :accessor :title)
   (url
    :initarg :url :accessor :url)
   (description
    :initarg :description :accessor :description)
   (timestamp
    :initarg :timestamp :accessor :timestamp)))


(defgeneric make-itemobject-list (feedconfig)
  (:documentation "feedconfigを受け取って、itemobjのリストを返す"))

(defmacro feedconfig->itemobj (feed-type &body body)
  "feedconfigからitemobjを取り出すためのメソッド関数を生成するマクロ"
  (let ((url-list (gensym)))
    `(defmethod make-itemobject-list ((feedconfig ,feed-type))
       (let ((,url-list (:url-list feedconfig)))
	 (loop for url in ,url-list
	       append ,@body)))))



(defmacro itemlist->itemobjects (item-list for &key title url timestamp description)
  `(loop for ,for in ,item-list
	 collect (let ((title ,title)
		       (url ,url)
		       (timestamp ,timestamp)
		       (description ,description))
		   (make-instance 'item
				  :title title
				  :url url
				  :description description
				  :timestamp timestamp))))

(feedconfig->itemobj wikidot-jp
  (let* ((source (fetch-and-parse-from-xml url))
	 (channel (find-content-if "channel" source))
	 (items (remove-content-if-not '("item") channel))
	 (item-list (mapcar
		     #'(lambda (i)
			 (remove-content-if-not
			  '("title" "link" "pubDate"
			    ("encoded" . "http://purl.org/rss/1.0/modules/content/"))
			  i))
		     items)))
    (itemlist->itemobjects item-list i
		   :title  (third (first i))
		   :url  (third (second i))
		   :timestamp (third (third i))
		   :description (third (fourth i)))))

(feedconfig->itemobj sandbox
  (let ((source (fetch-and-parse-from-xml url)))))



;; テストコード

;; (defparameter *items* (make-itemobject-list *wikidot-jp-rss-link*))

;; (mapcar #':timestamp *items*)


;;;; ------------------------------------------------------------------
;;;; メインループ
;;;; ------------------------------------------------------------------



(defun format-rss-message (item color icon)
  "RSSアイテムをDiscordのEmbedデータ（連想リスト）に変換する"
  `(("title" . ,(format nil "~a ~a" icon (getf item :title)))
    ("description" . ,(let ((desc (getf item :description)))
			;; descriptionが長すぎる場合に400文字で切り詰める
			(if (> (length desc) 400)
			    (concatenate 'string (subseq desc 0 400) "...")
			    desc)))
    ("url" . ,(getf item :link))
    ("color" . ,color) ; 色を16進数の #3498DB (青色) から10進数に変換したもの
    ("timestamp" . ,(local-time:format-timestring nil (local-time:now))) ; 現在時刻のタイムスタンプ
    ("footer" . (("text" . "RSS Feed Bot")))))




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
;;;; bot-commands
;;;; --------------------------------------------------------------



(defcommand :rss :post arg
  )

(defcommand :rss :initialize arg
  (set-bot-token *token-filepath*)
  (set-seen-items *queue-list-filepath*))

(defcommand :rss :save-queue arg
  (output-key-plist-to-data *key-queue*))


