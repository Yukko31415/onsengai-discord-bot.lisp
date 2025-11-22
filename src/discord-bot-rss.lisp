(in-package #:discord-bot-rss)


;;;; ------------------------------------------------------------------
;;;; classの設定
;;;; ------------------------------------------------------------------



(defclass article ()
  ((link-list
    :initarg :link :accessor :link-list)
   (color
    :initarg :color :accessor :color)
   (icon
    :initarg :icon :accessor :icon)))


(defclass sandbox (article)
  ((color
    :initform 49408)
   (icon
    :initform "<:SB:1088712219656728657>")))


(defclass wikidot-jp (article)
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
	   (list (read content))
	   (keys-list list))

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
		 :link '("http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml")))



(defparameter *wikidot-jp-rss-link*
  (make-instance 'wikidot-jp
		 :link '("http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml")))





;;;; ------------------------------------------------------------------
;;;; RSSフィードの取得と解析 ------------------------------------------
;;;; ------------------------------------------------------------------


(defun get-item-list (depth key url)
  (find-if
   #'(lambda (list) (and (consp list) (stringp (car list)) (string= (car list) key)))
   (nth depth (xmls:parse-to-list (dex:get url)))))

(defun get-item-car (car-item)
  (if (consp car-item)
      (get-item-car (car car-item))
      car-item))

(defun item-member-p (item list)
  (let ((car-item (car item)))
    (member (get-item-car car-item)
        list :test 'equal)))

(defun get-item-contents (depth key list url)
  (remove-if-not
   #'(lambda (item) (and (consp item)
             (item-member-p item list)))
   (get-item-list depth key url)))


;;;; ------------------------------------------------------------------
;;;; メインループ -----------------------------------------------------
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


