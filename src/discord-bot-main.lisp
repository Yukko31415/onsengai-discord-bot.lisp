

(in-package #:discord-bot-main)




;;;; --------------------------------------------------------------
;;;; main ---------------------------------------------------------


(defun make-loop-list (list)
  (setf (cdr (last list)) list))


(defun command-player (command-list)
  (let ((command (car command-list))
	(arg (cdr command-list)))
    (bot-command command arg)))




(defun main ()
  (loop #:do (let ((command (get-command)))
	       (progn
		 (make-thread #'(lambda ()
				  (command-player command)))
		 (bot-command :sleep '(0.5))))))








;;;; --------------------------------------------------------------
;;;; start/end ----------------------------------------------------


(defun boot ()
  (progn (add-command (:initialize)))
  (start-rss-bot))

(defun random-in-range (min max)
  "Generates a random integer within the range [min, max]."
  (+ min (random (1+ (- max min)))))

(defun start ()
  (slynk:create-server :port (random-in-range 1000 65535) :dont-close t)
  (join-thread (make-thread
		#'(lambda ()
		    (progn
		      (clear-command)
		      (boot)
		      (catch :end-flag (main))
		      (format t "bot closed~%"))))))


(defmethod bot-command ((key (eql :end)) arg)
  (throw :end-flag t))

(defun end ()
  (add-command (:end)))

