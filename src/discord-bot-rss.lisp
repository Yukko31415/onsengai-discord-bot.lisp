(in-package #:discord-bot-rss)





;;
;; config


(defparameter *channel-id* "1406525194289287311" "投稿先のDiscordチャンネルID")

(defvar *interval* 300 "RSSフィードのチェック間隔（秒）")

(defparameter *max-items* 30)



(defparameter *queue-list-filepath*
  (merge-pathnames "common-lisp/discord-bot/data/rss-queue-list.txt"
		   (user-homedir-pathname)))


(rss-parser:define-rss-fetcher sandbox
  "http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml"
  ("title" "pubDate" "description" "guid")
  :key "guid"
  :size *max-items*)


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



(defclass rss-bot ()
  ((activep :initform nil
	    :accessor rss-bot-activep)
   (enablep :initform nil
	    :accessor rss-bot-enablep)))


;; 
;; handler


(defun make-rss-bot ()
  "rss-botインスタンスを生成する"
  (make-instance 'rss-bot))


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









