(in-package #:discord-bot-rss)



;;
;; config


(defparameter *channel-id* "1121439803213365279" "投稿先のDiscordチャンネルID")

(defvar *interval* 300 "RSSフィードのチェック間隔（秒）")

(defparameter *max-items* 30)



(defparameter *queue-list-filepath*
  (merge-pathnames "common-lisp/discord-bot/data/rss-queue-list.txt"
		   (user-homedir-pathname)))


(rss:define-fetcher sandbox
  "http://scp-jp-sandbox3.wikidot.com/feed/pages/tags/%2B_criticism-in/category/draft/order/updated_at%20.xml"
  ("title" "pubDate" "description" "guid")
  :key "guid"
  :size *max-items*)

(rss:define-fetcher wikidot-jp
  "http://scp-jp.wikidot.com/feed/pages/category/_default%2Cauthor%2Cprotected%2Cwanderers%2Ctheme%2Ccomponent%2Creference%2Cart/order/created_at%20desc/limit/20.xml"
  ("title" "pubDate" "description" "guid")
  :key "guid"
  :size *max-items*)

(defparameter *fetch-list* (mapcar #'rss:make-fetcher '(sandbox wikidot-jp)))




;;
;; %post-item


(defgeneric %post-item (item))


(defmethod %post-item ((item sandbox))
  (let ((title (title item))
	(pubdate (pubdate item))
	(description (description item))
	(guid (guid item)))
    (log:info "Post: [~a](~a)" title guid)
    (send-discord-message *channel-id* *bot-token*
     :content pubdate
     :embeds ((embed :title (format nil "<:SB:1088712219656728657> ~A" title)
		     :url guid
		     :color 49408
		     :timestamp (local-time:format-rfc3339-timestring nil (local-time:now))
		     :footer (footer "RSS Bot"))))))



(defmethod %post-item ((item wikidot-jp))
  (let ((title (title item))
	(pubdate (pubdate item))
	(description (description item))
	(guid (guid item)))
    (log:info "Post: [~a](~a)" title guid)
    (send-discord-message *channel-id* *bot-token*
     :content pubdate
     :embeds ((embed :title (format nil "<:Pos:1088712480865394700> ~A" title)
		     :url guid
		     :color 15007744
		     :timestamp (local-time:format-rfc3339-timestring nil (local-time:now))
		     :footer (footer "RSS Bot"))))))


;;
;; post-item


(defun post-item (queue fetcher-amount)
  (loop :with count := fetcher-amount
	:for item := (lparallel.queue:pop-queue queue)
	:if (keywordp item)
	  :do (ecase item (:end (decf count)))
	:else
	  :do (%post-item item) (sleep 1)
	:until (zerop count)
	:finally (log:info "post-item was closed")))


;;
;; fetch-and-post


(defstruct (retry-counter (:conc-name nil))
  (retry-count 0 :type (integer 0 10)))



(defun %fetch-and-post (fetcher)
  (loop (multiple-value-bind (val status)
	    ;; status: nil -> retry, t -> finish
	    (restart-case
		(progn (log:info "fetch: ~a" (rss:fetcher-name fetcher))
		       (values (rss:fetch fetcher) t))
	      (retry (count) (log:info "retry: ~a" count) (sleep (* (expt count 2) 10)) (values nil nil))
	      (giveup () (log:info "諦めます") (values nil t)))
	  (when status (return val)))))


(defun fetch-and-post-handler (c retry-counter)
  (log:warn "Error: ~a" c)
  (let ((counter (retry-count retry-counter)))
    (if (>= 3 counter)
	(progn (incf (retry-count retry-counter))
	       (invoke-restart 'retry counter))
	(invoke-restart 'giveup))))



(defun fetch-and-post (fetcher queue)
  (log:info "run: ~a" (rss:fetcher-name fetcher))
  (let ((retry-counter (make-retry-counter)))
    (handler-bind ((error #'(lambda (c) (fetch-and-post-handler c retry-counter))))
      (loop :for item :in (nreverse (%fetch-and-post fetcher))
	    :do (lparallel.queue:push-queue item queue)
	    :finally (lparallel.queue:push-queue :end queue)))))


;;
;; run-rss


(defun run-rss ()
  (loop :with queue := (lparallel.queue:make-queue)
	:initially (bt:make-thread #'(lambda () (post-item queue (length *fetch-list*))))
	:for fetcher :in *fetch-list*
	:do (let ((fetcher fetcher))
	      (bt:make-thread #'(lambda () (fetch-and-post fetcher queue))))))



;;
;; rss-loop


(defclass rss-bot ()
  ((activep :initform nil
	    :accessor rss-bot-activep)
   (enablep :initform nil
	    :accessor rss-bot-enablep)))


(defun save-cache (&optional stream)
  "streamにキャッシュを書き込む"
  (let ((*print-readably* t))
    (flet ((print-cache (fetcher)
	     (rss:print-cache-queue fetcher stream)))
      (mapc #'print-cache *fetch-list*))))


(defun %rss-loop (rss-bot interval &aux (times 0))
  (declare (fixnum interval))
  (declare (rss-bot rss-bot))
  (loop :while (rss-bot-enablep rss-bot)
	:do (progn (incf times)
		   (log:info "~A回目のループ" times)
		   (run-rss)
		   (when (= (mod times 5) 0)
		     (with-open-file
			 (stream *queue-list-filepath* :direction :output
						       :if-exists :supersede)
		       (save-cache stream))
		     (log:info "cache was backuped"))
		   (sleep interval))))



(defun rss-loop (rss-bot)
  (setf (rss-bot-activep rss-bot) t)
  (%rss-loop rss-bot *interval*)
  (setf (rss-bot-activep rss-bot) nil))


;; 
;; bot-handler


(defun make-rss-bot ()
  "rss-botインスタンスを生成する"
  (make-instance 'rss-bot))


(defun stop (rss-bot)
  "rss-botを停止する"
  (declare (rss-bot rss-bot))
  (setf  (rss-bot-enablep rss-bot) nil))


(defun run (rss-bot)
  (setf (rss-bot-enablep rss-bot) t)
  (unless (rss-bot-activep rss-bot)
    (progn (with-open-file (stream *queue-list-filepath*)
	     (loop :for fetcher :in *fetch-list*
		   :do (rss:initialize-fetcher-cache
			fetcher (read stream))))
	   (log:info "chace was initialized")
	   (bt:make-thread #'(lambda () (rss-loop rss-bot))))))
