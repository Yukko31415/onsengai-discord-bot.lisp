

(in-package #:discord-bot-main)



;;;; --------------------------------------------------------------
;;;; start/end ----------------------------------------------------


(defun random-in-range (min max)
  "Generates a random integer within the range [min, max]."
  (+ min (random (1+ (- max min)))))

(defun start ()
  "botを起動する。スレッドを作成し、boot関数を叩く"
  (slynk:create-server :port (random-in-range 1000 65535) :dont-close t)
  (join-thread (make-thread
		#'(lambda () (boot)))))


(defun boot ())


