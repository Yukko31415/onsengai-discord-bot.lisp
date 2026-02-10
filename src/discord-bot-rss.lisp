(in-package #:discord-bot-rss)


<<<<<<< HEAD
;;;; ------------------------------------------------------------------
;;;; feedconfig
;;;; ------------------------------------------------------------------
=======
>>>>>>> sub



;;
;; config


(defparameter *channel-id* "1406525194289287311" "投稿先のDiscordチャンネルID")

(defvar *interval* 300 "RSSフィードのチェック間隔（秒）")

(defparameter *max-items* 30)



(defparameter *queue-list-filepath*
  (merge-pathnames "common-lisp/discord-bot/data/rss-queue-list.txt"
		   (user-homedir-pathname)))


<<<<<<< HEAD
(defparameter *key-queue* (make-queue))
=======
(rss-parser:define-rss-fetcher sandbox
  "http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml"
  ("title" "pubDate" "description" "guid")
  :key "guid"
  :size *max-items*)

>>>>>>> sub

(rss-parser:define-rss-fetcher wikidot-jp
  "http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml"
  ("title" "pubDate" "description" "guid")
  :key "guid"
  :size *max-items*)

(defparameter *fetch-list* (mapcar #'rss-parser:make-rss-fetcher '(sandbox wikidot-jp)))




;;
;; post-item


(defgeneric post-item (item))

(defmethod post-item :around (item)
  (sleep 1)
  (call-next-method))


(defmethod post-item ((item sandbox))
  (let ((title (title item))
	(pubdate (pubdate item))
	(description (description item))
	(guid (guid item)))
    (send-discord-message
     *channel-id* discord-bot-token:*bot-token*
     :content pubdate
     :embeds ((embed :title (format nil "<:SB:1088712219656728657> ~A" title)
		     :url guid
		     :color 49408
		     :timestamp (local-time:format-rfc3339-timestring nil (local-time:now))
		     :footer (footer "RSS Bot"))))))



(defmethod post-item ((item wikidot-jp))
  (let ((title (title item))
	(pubdate (pubdate item))
	(description (description item))
	(guid (guid item)))
    (send-discord-message
     *channel-id* discord-bot-token:*bot-token*
     :content pubdate
     :embeds ((embed :title (format nil "<:Pos:1088712480865394700> ~A" title)
		     :url guid
		     :color 15007744
		     :timestamp (local-time:format-rfc3339-timestring nil (local-time:now))
		     :footer (footer "RSS Bot"))))))


(defun run-rss  ()
  (mapcar #'post-item
	  (nreverse (loop :for fetcher :in *fetch-list*
			  :append (rss-parser:fetch fetcher)))))


(defun save-cache (&optional stream)
  (let ((*print-readably* t))
    (flet ((print-cache (fetcher)
	     (rss-parser:print-rss-cache-queue fetcher stream)))
      (mapc #'print-cache *fetch-list*))))



(define-condition start-fetch (condition)
  ((times :initarg :times :reader fetch-times)))




(defun rss-loop (rss-bot interval &aux (times 0))
  (declare (fixnum interval))
  (loop :do (restart-case
		(progn (incf times)
		       (log:info "~A回目のループ" times)
		       (signal 'start-fetch :times times)
		       (run-rss)
		       (when (= (mod times 5) 0)
			 (with-open-file
			     (stream *queue-list-filepath* :direction :output)
			   (save-cache stream)))
		       (sleep interval))
	      (loop-finish ()
		:report "メインループを終了する"
		(log:info "ループを終了します")
		(return (setf (rss-bot-activep rss-bot) nil)))
	      (retry ()
		:report "リトライを行う"
		(log:info "リトライを行います")
		nil))))

<<<<<<< HEAD
;;;; ------------------------------------------------------------------
;;;; feedconfig->item obj
;;;; ------------------------------------------------------------------
=======

(defun loop-handler (rss-bot)
  (setf (rss-bot-activep rss-bot) t)
  (handler-bind ((start-fetch
		   #'(lambda (c) (declare (ignore c))
		       (if (rss-bot-enablep rss-bot)
			   nil
			   (invoke-restart 'loop-finish))))
		 (error
		   #'(lambda (c) (log:warn "Error: ~a" c)
		       (invoke-restart 'retry))))
    (rss-loop rss-bot *interval*)))


>>>>>>> sub

(defclass rss-bot ()
  ((activep :initform nil
	    :accessor rss-bot-activep)
   (enablep :initform nil
	    :accessor rss-bot-enablep)))

<<<<<<< HEAD

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
=======

;; 
;; handler


(defun make-rss-bot ()
  "rss-botインスタンスを生成する"
  (make-instance 'rss-bot))
>>>>>>> sub


(defun stop (rss-bot)
  ""
  (declare (rss-bot rss-bot))
  (setf  (rss-bot-enablep rss-bot) nil))


(defun run (rss-bot)
  (setf (rss-bot-enablep rss-bot) t)
  (if (rss-bot-activep rss-bot)
      nil
      (progn
	(with-open-file (stream *queue-list-filepath*)
	  (loop :for fetcher :in *fetch-list*
		:do (rss-parser:initialize-fetcher-cache
		     fetcher (read stream))))
	(bt:make-thread #'(lambda () (loop-handler rss-bot))))))



<<<<<<< HEAD
(feedconfig->itemobj (sandbox url)
  (let* ((source (fetch-and-parse-from-xml url))
	 (channel (find-content "channel" source))
	 (items (remove-content-if-not '("item") channel))
	 (item-list (extract-item-list
		     '("title" "link" "pubDate" "encoded")
		     items)))

    (log:info "~a~%" items)

    (mapcar #'(lambda (i)
		(make-instance 'item
			       :title (third (first i))
			       :url (third (second i))
			       :description (third (fourth i))
			       :timestamp (third (third i))))
	    item-list)))





;;;; ------------------------------------------------------------------
;;;; format-itemobj
;;;; ------------------------------------------------------------------
=======
>>>>>>> sub






<<<<<<< HEAD

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
    
    (list (item-timestamp itemobj)
	  (format-rss-message itemobj color icon description))))




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
  "urlをキーとして受け取り、それが既に送信したものかを確認する"
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

=======
>>>>>>> sub
