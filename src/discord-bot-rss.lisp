(in-package #:discord-bot-rss)


;;;; ------------------------------------------------------------------
;;;; classの設定
;;;; ------------------------------------------------------------------



(defclass feedconfig ()
  ((url-list
    :initarg :url-list :accessor feedconfig-url-list)
   (color
    :initarg :color :accessor feedconfig-color)
   (icon
    :initarg :icon :accessor feedconfig-icon)
   (channel
    :initform 1406525194289287311
    :accessor feedconfig-channel)))


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




(defparameter *queue-list-filepath*
  (merge-pathnames "common-lisp/discord-bot/data/rss-queue-list.txt"
		   (user-homedir-pathname)))



(defparameter *key-queue* (make-queue))


(defparameter *seen-items* (make-hash-table :test 'equal))

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

      
      (setf *key-queue* queue)
      (setf *seen-items* hash-table))))



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

(defun get-tag (arg)
  (if (consp arg)
      (get-tag (car arg))
      arg))

(defun find-content (key list)
  "受け取ったリストの子ノードを解析し、一致したキーを持つリストを返す"
  (find key (cdr list)
        :test #'string=
        :key #'get-tag))

(defun remove-content-if-not (keys-list item)
  "キーに一致しない子ノードを取り除いて返す"
  (remove-if-not
   #'(lambda (s) (member s keys-list :test #'string=))
   item
   :key #'get-tag))


;; test
;; (remove-content-if-not ("item") (find-content "channel" (fetch-and-parse-from-xml "http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml")))



;;;; ------------------------------------------------------------------
;;;; feedconfig->item obj
;;;; ------------------------------------------------------------------


(defclass item ()
  ((title
    :initarg :title :accessor item-title)
   (url
    :initarg :url :accessor item-url)
   (description
    :initarg :description :accessor item-description)
   (timestamp
    :initarg :timestamp :accessor item-timestamp)))

(defgeneric make-itemobject-list (feedconfig)
  (:documentation "feedconfigを受け取って、itemobjのリストを返す"))


(defmacro feedconfig->itemobj ((feed-type url-for) &body body)
  "feedconfigからitemobjを取り出すためのメソッド関数を生成するマクロ"
  (let ((url-list (gensym)))
    `(defmethod make-itemobject-list ((feedconfig ,feed-type))
       (let ((,url-list (feedconfig-url-list feedconfig)))
	 (loop for ,url-for in ,url-list
	       append ,@body)))))


(defun extract-item-list (keys-list item-list)
  "xmlパースされたitemのリストを受け取り、kyes-listに該当するリストのみを抽出する"
  (mapcar #'(lambda (i) (remove-content-if-not keys-list i))
	  item-list))


(feedconfig->itemobj (wikidot-jp url)
  (let* ((source (fetch-and-parse-from-xml url))
	 (channel (find-content "channel" source))
	 (items (remove-content-if-not '("item") channel))
	 (item-list (extract-item-list
		     '("title" "link" "pubDate" "encoded")
		     items)))

    (mapcar #'(lambda (i)
		(make-instance 'item
			       :title (third (first i))
			       :url (third (second i))
			       :description (third (fourth i))
			       :timestamp (third (third i))))
	    item-list)))


(feedconfig->itemobj (sandbox url)
  (let* ((source (fetch-and-parse-from-xml url))
	 (channel (find-content "channel" source))
	 (items (remove-content-if-not '("item") channel))
	 (item-list (extract-item-list
		     '("title" "link" "pubDate" "encoded")
		     items)))

    (log:info "~a" items)

    (mapcar #'(lambda (i)
		(make-instance 'item
			       :title (third (first i))
			       :url (third (second i))
			       :description (third (fourth i))
			       :timestamp (third (third i))))
	    item-list)))


;; testcode
;; (defparameter *items* (make-itemobject-list *sandbox-rss-link*))
;; (mapcar #'item-url *items*)


;;;; ------------------------------------------------------------------
;;;; format-itemobj
;;;; ------------------------------------------------------------------




(defun format-rss-message (itemobj color icon description)
  "RSSアイテムをDiscordのEmbedデータ（連想リスト）に変換する"
  `(:title ,(format nil "~a ~a" icon (item-title itemobj))
    :description ,description
    :url ,(item-url itemobj)
    :color ,(write-to-string color)
    :timestamp ,(local-time:format-timestring nil (local-time:now))
    :footer (:|text| "RSS Feed Bot")))


(defgeneric format-itemobj (feedconfig itemobj)
  (:documentation "feedconfigとitemobjを受け取り、メッセージ送信用にフォーマットされたリストを返す"))


(defmethod format-itemobj ((feedconfig wikidot-jp) itemobj)
  (let ((color (feedconfig-color feedconfig))
	(icon (feedconfig-icon feedconfig)))
    (list (item-timestamp itemobj)
	  (format-rss-message itemobj color icon ""))))


(defmacro search-text-from-node (itemobj tag sub-sequences-list)
  "指定したタグ内のテキストを探し、一致したものを返す"
  (let* ((text (gensym))
	 (node (gensym))
	 (search-list
	   (mapcar
	    #'(lambda (sub-sequence) (list 'search sub-sequence text))
	    sub-sequences-list)))
    `(lquery:$ (initialize (item-description ,itemobj))
       ,tag
       (filter (lambda (,node)
		 (let ((,text (plump:text ,node)))
		   (or ,@search-list))))
       (parent)
       (text))))


(defmethod format-itemobj ((feedconfig sandbox) itemobj)
  (log:info "~a" (item-description itemobj))
  (let* ((color (feedconfig-color feedconfig))
	 (icon (feedconfig-icon feedconfig))
	 (%description (search-text-from-node
			itemobj "strong" ("付与予定タグ:" "付与予定タグ: ")))
	 (description (let ((description (aref %description 0)))
			(if (stringp description)
			    description
			    ""))))
    
    (log:info "~s~%" %description)
    
    (list (item-timestamp itemobj)
	  (format-rss-message itemobj color icon description))))


;; testcode
;; (let ((message (format-itemobj *sandbox-rss-link* (fifth *items*))))
;;   (run-command  (:post :post-message 1406525194289287311 message)))



;;;; ------------------------------------------------------------------
;;;; send-rss-message-to-discord
;;;; ------------------------------------------------------------------


(defun add-data (key)
  "新しいデータを追加し、上限を超えていれば最も古いデータを削除する"
  ;; 新しいデータを追加
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


(defun send-rss-message-to-discord (channel feedconfig itemobj)
  "seen-items-pがnilの時、メッセージを送信する"
  (let ((url (item-url itemobj)))
    (unless (seen-items-p url)
      (progn (add-data url)
	     (let ((message (format-itemobj feedconfig itemobj)))
	       (run-command (:post :post-message channel message))
	       (sleep 1))))))


;;;; ------------------------------------------------------------------
;;;; main
;;;; ------------------------------------------------------------------



(defun check-and-send-rss (feedconfig)
  (let ((itemobj-list (make-itemobject-list feedconfig))
	(channel (feedconfig-channel feedconfig)))
    (mapc
     #'(lambda (item)
	 (send-rss-message-to-discord channel feedconfig item))
     itemobj-list)))


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

(defcommand :rss :save-queue arg
  (output-key-plist-to-data *key-queue*))

(defcommand :rss :initialize arg
  (set-seen-items *queue-list-filepath*))

;;;; test

(defun test ()
  (setf *key-queue* (make-queue))
  (setf *seen-items* (make-hash-table :test 'equal))
  (run-command (:rss :post)))
