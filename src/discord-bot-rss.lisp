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
    :initarg :icon :accessor :icon)
   (channel
    :initform 1406525194289287311
    :accessor :channel)))


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
	(setf (gethash key hash-table) key)
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


(defparameter *feedconfig-list* (list *sandbox-rss-link* *wikidot-jp-rss-link*))



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


;; testcode
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
  "feedconfigからitemobjを取り出すためのメソッド関数を生成するマクロ
   ! urlがすでに束縛されている変数として機能している"
  (let ((url-list (gensym)))
    `(defmethod make-itemobject-list ((feedconfig ,feed-type))
       (let ((,url-list (:url-list feedconfig)))
	 (loop for url in ,url-list
	       append ,@body)))))


(defmacro itemlist->itemobjects (item-list for &key title url timestamp description)
  "item-listを受け取り、itemobjectのリストを返す"
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
	 (item-list (mapcarx
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


;; テストコード

;; (defparameter *items* (make-itemobject-list *sandbox-rss-link*))

;; (mapcar #':url *items*)


;;;; ------------------------------------------------------------------
;;;; format-itemobj
;;;; ------------------------------------------------------------------


(defun format-rss-message (itemobj color icon description)
  "RSSアイテムをDiscordのEmbedデータ（連想リスト）に変換する"
  `((#:title . ,(format nil "~a ~a" icon (:title itemobj)))
    (#:description . ,description)
    (#:url . ,(:url itemobj))
    (#:color . ,color) ; 色を16進数の #3498DB (青色) から10進数に変換したもの
    (#:timestamp . ,(local-time:format-timestring nil (local-time:now))) ; 現在時刻のタイムスタンプ
    (#:footer . (("text" . "RSS Feed Bot")))))


(defgeneric format-itemobj (feedconfig itemobj)
  (:documentation "feedconfigとitemobjを受け取り、メッセージ送信用にフォーマットされたリストを返す"))


(defmethod format-itemobj ((feedconfig wikidot-jp) itemobj)
  (let ((color (:color feedconfig))
	(icon (:icon feedconfig)))
    (format-rss-message itemobj color icon "")))


(defmacro search-text-from-node (itemobj tag sub-sequences-list)
  "指定したタグ内のテキストを探し、一致したものを返す"
  (let* ((text (gensym))
	 (node (gensym))
	 (search-list (mapcar #'(lambda (sub-sequence) (list 'search sub-sequence text))
			      sub-sequences-list)))
    `(lquery:$ (initialize (:description ,itemobj))
       ,tag
       (filter (lambda (,node)
		 (let ((,text (plump:text ,node)))
		   (or ,@search-list))))
       (parent)
       (text))))


(defmethod format-itemobj ((feedconfig sandbox) itemobj)
  (let ((color (:color feedconfig))
	(icon (:icon feedconfig))
	(description (aref (search-text-from-node
			    itemobj "strong" ("付与予定タグ:" "付与予定タグ: "))
			    0)))
    (list (:timestamp itemobj) (format-rss-message itemobj color icon description))))



;; testcode
;; (let ((message (format-itemobj *sandbox-rss-link* (fifth *items*))))
;;   (run-command  (:post :post-message 1406525194289287311 message)))


;;;; ------------------------------------------------------------------
;;;; send-rss-message-to-descord
;;;; ------------------------------------------------------------------


(defun add-data (key)
  "新しいデータを追加し、上限を超えていれば最も古いデータを削除する"
  ;; ステップ1: 新しいデータを追加
  (setf (gethash key *seen-items*) key)
  ;; 新しいキーをキューの末尾に追加
  (push-queue key *key-queue*)
  
  ;; 上限を超えているかチェック
  (loop
    #:do (if (> (queue-count *key-queue*) *max-items*)
	     ;; 超えていれば、一番古いデータを削除
	     ;; キューの先頭から一番古いキーを取得
	     (let ((oldest-key (pop-queue *key-queue*)))
	       ;; ハッシュテーブルからそのキーのデータを削除
	       (remhash oldest-key *seen-items*))
	     (return))))

(defun seen-items-p (key)
  "urlをキーとして受け取り、それが既に送信したものかを確認する述語"
  (when (gethash key *seen-items*) t))


(defun send-rss-message-to-descord (channel feedconfig itemobj)
  "seen-items-pがnilの時、メッセージを送信する"
  (let ((url (:url itemobj)))
    (unless (seen-items-p url)
      (progn (add-data url)
	     (let ((message (format-itemobj feedconfig itemobj)))
	       (run-command (:post :post-message channel message)))))))




;;;; ------------------------------------------------------------------
;;;; main
;;;; ------------------------------------------------------------------


(defun check-and-send-rss (feedconfig)
  (let ((itemobj-list (make-itemobject-list feedconfig))
	(channel (:channel feedconfig)))
    (mapc
     #'(lambda (item) (send-rss-message-to-descord channel feedconfig item)) itemobj-list)))


(defun main (feedconfig-list)
  (mapc #'check-and-send-rss feedconfig-list))


;;;; ------------------------------------------------------------------
;;;; utils
;;;; ------------------------------------------------------------------



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
  (main *feedconfig-list*))

(defcommand :rss :initialize arg
  (set-seen-items *queue-list-filepath*))

(defcommand :rss :save-queue arg
  (output-key-plist-to-data *key-queue*))

