

(in-package #:discord-bot-main)






;;;; --------------------------------------------------------------
;;;; start/end ----------------------------------------------------


(defun random-in-range (min max)
  "Generates a random integer within the range [min, max]."
  (+ min (random (1+ (- max min)))))


(defcommand :initialize arg
  (initialize)
  (run-rss-bot))


(defun boot ()
  (progn (add-command (:initialize))))

(defun start ()
  (slynk:create-server :port (random-in-range 1000 65535) :dont-close t)
  (join-thread (make-thread
		#'(lambda () (boot)))))


(defmethod bot-command ((key (eql :end)) arg)
  (throw :end-flag t))

(defun end ()
  (add-command (:end)))

