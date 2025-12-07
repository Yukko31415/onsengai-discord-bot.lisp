

(in-package #:discord-bot-main)



;;;; --------------------------------------------------------------
;;;; start/end
;;;; --------------------------------------------------------------


(defparameter *initialize-list* '(:token :rss))

(defun initialize ()
  "各パッケージのinitializeコマンドを実行する"
  (mapc #'(lambda (key)
	    (run-command (key :initialize)))
	*initialize-list*))


(defun boot ()
  "各パッケージのbotを起動させる"
  (run-command (:main :command-list
		      (:main :command-dotimes 3 (:rss :post) (:main :sleep 300))
		      (:rss :save-queue))))




(defun random-in-range (min max)
  "Generates a random integer within the range [min, max]."
  (+ min (random (1+ (- max min)))))

(defun start ()
  "botを起動する。スレッドを作成し、boot関数を叩く"
  (progn (initialize)
	 (boot)
	 (slynk:create-server :port (random-in-range 1000 65535) :dont-close t)))

(defun end ())
