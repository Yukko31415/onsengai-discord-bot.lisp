

(in-package #:discord-bot-preloads)


;;;; ---------------------------------------------------------------------------------
;;;; bot-command ---------------------------------------------------------------------


(defmethod bot-command ((key (eql :initialize)) arg)
  (format t "初期化中……")
  (initialize))


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


